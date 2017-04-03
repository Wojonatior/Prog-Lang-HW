#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "hw06-env-values.rkt")

;===============================================================================
;========================= Lexical and Grammar Specs ===========================
;===============================================================================

(define lexical-spec
  '(
    (whitespace (whitespace) skip)
    (comment ("#" (arbno (not #\newline))) skip)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    )
  )

;write your answer in string form:
(define problem-1-answer
  "The strings are chopped up by 4 main categories. The first category is
 whitespace, which is ignored for parsing. The second category is a
a comment category which is ended only by the newline character and ignored too.
There are two types of numbers, positive numbers and negative numbers with one
matching format for each of them. And the last one an itentifier which is an
arbitrary string that has to start with a letter, but can contain numbers, -, _, and ?"
  )

(define grammar-spec
  '(
    (program (expr (arbno expr)) a-program)
    (expr (number) num-expr)
    (expr ("up(" expr ")") up-expr)
    (expr ("down(" expr ")") down-expr)
    (expr ("left(" expr ")") left-expr)
    (expr ("right(" expr ")") right-expr)

    (expr ("(" expr expr ")") point-expr)
    (expr ("+" expr expr) add-expr)
    (expr ("origin?" "(" expr ")") origin-expr)
    (expr ("if" "(" expr ")" "then" expr "else" expr) if-expr)
    (expr("move" "(" expr expr (arbno expr) ")") move-expr)

    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
    (expr (identifier) iden-expr)
    (var-expr ("val" identifier "=" expr) val)
    (var-expr ("final val" identifier "=" expr) final-val)))

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
  (flatten (list el1 rest))
  )
;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;value-of takes as a parameter an AST resulted from a call to the
;parser function.
(define (run program-string)
  (value-of (parser program-string) (empty-env))
)

(define (value-of ast env)
  (cond
    ((program? ast)
     (value-of-program ast env))
    ((expr? ast)
     (value-of-expr ast env))
    ((var-expr? ast)
     (value-of-var ast env))
    (else 0)))

(define (value-of-program ast env)
  (cases program ast
    (a-program (expr exprs)
      (andmap (lambda (ex) (value-of expr env))
         (flat-list expr exprs)))))

(define (value-of-expr ast env)
  (cases expr ast
         (num-expr (num) (eval-num num env))
         (down-expr (expr1) (eval-step down-step expr1 env))
         (up-expr (expr1) (eval-step up-step expr1 env))
         (left-expr (expr1) (eval-step left-step expr1 env))
         (right-expr (expr1) (eval-step right-step expr1 env))
         (point-expr (expr1 expr2) (eval-point expr1 expr2 env))
         (origin-expr (expr1) (eval-origin expr1 env))
         (if-expr (cond-expr expr1 expr2) (eval-if cond-expr expr1 expr2 env))
         (add-expr (expr1 expr2) (eval-add expr1 expr2 env))
         (move-expr (expr1 expr2 exprs) (eval-move expr1 expr2 exprs env))
         (iden-expr (var-name) (apply-env env var-name))
         (block-expr
           (lst-of-var-expr lst-of-expr)
           (andmap (lambda (x) (value-of x (new-env lst-of-var-expr env)))  lst-of-expr))))

(define (value-of-var v-ex env)
  (or (var-expr? v-ex) (invalid-args-exception "value-of-var" "var-expr?" v-ex))
  (cases var-expr v-ex
    (val
     (iden val-of-iden)
     (extend-env-wrapper iden (value-of val-of-iden env) env NON-FINAL))
    
    (final-val
     (iden val-of-iden)
     (extend-env-wrapper iden (value-of val-of-iden env) env FINAL))))
    

(define (eval-num num env)
  (num-val num))

(define (eval-step step-type expr1 env)
  (letrec
    ([val1 (value-of expr1 env)])
    (step-val (step-type (num-val->n val1)))))

(define (eval-point expr1 expr2 env)
  (letrec ([val1 (value-of expr1 env)]
           [val2 (value-of expr2 env)])
    (point-val
      (point (num-val->n val1) (num-val->n val2)))))

(define (eval-origin expr1 env)
  (letrec ([cord (point-val->p (value-of expr1 env))])
    (bool-val
      (and (= 0 (point->x cord))
           (= 0 (point->y cord))))))

(define (eval-if bool-expr expr1 expr2 env)
  (letrec ([ifbool (value-of bool-expr env)]
           [val1 (value-of expr1 env)]
           [val2 (value-of expr2 env)])
    (if
      (bool-val->b ifbool)
      val1
      val2)))

(define (eval-move expr1 expr2 exprs env)
  (letrec ([start (value-of expr1 env)])
    (foldl 
      (lambda (arg1 arg2) (move arg1 arg2 env))
      start
      (cons expr2 exprs))))

(define (move expr1 expr2 env)
  (letrec
    ([step (step-val->st (value-of expr1 env))]
     [p (point-val->p expr2)]
     [n (single-step->n step)]
     [x (point->x p)]
     [y (point->y p)])
    (point-val
      (cond
        ((up-step? step) (point x (+ n y)))
        ((down-step? step) (point x (- y n)))
        ((left-step? step) (point (- x n) y))
        ((right-step? step) (point (+ n x) y))))))

(define (eval-add expr1 expr2 env)
 (letrec
     ([l-step-val (value-of expr1 env)]
      [r-step-val (value-of expr2 env)]
      [l-step (step-val->st l-step-val)]
      [r-step (step-val->st r-step-val)]
      [res (+ (get-signed-value l-step) (get-signed-value  r-step))])
   (cond
     [(and (valid-steps? l-step r-step) (or (left-step? l-step) (right-step? l-step))) 
      (get-horizontal res)]
     [(and (valid-steps? l-step r-step) (or (up-step? l-step) (down-step? l-step)))
      (get-vertical res)]
     [else (raise "invalid args")])))

(define (valid-steps? st1 st2)
  (or 
   (and (up-step? st1) (up-step? st2))
   (and (down-step? st1) (down-step? st2))
   (and (up-step? st1) (down-step? st2))
   (and (down-step? st1) (up-step? st2))
   
   (and (left-step? st1) (left-step? st2))
   (and (right-step? st1) (right-step? st2))
   (and (left-step? st1) (right-step? st2))
   (and (right-step? st1) (left-step? st2))))

 (define (get-signed-value st)
  (cases step st
    (up-step (st) st)
    (down-step (st) (* -1 st))
    (right-step (st) st)
    (left-step (st) (* -1 st))))

 (define (get-vertical num)
  (if (positive? num)
      (step-val (up-step num)) 
      (step-val (down-step (* -1 num)))))

 (define (get-horizontal num)
  (if (positive? num)
      (step-val (right-step num)) 
      (step-val (left-step (* -1 num)))))


(define (new-env lst-var-expr old-env)
 (if (null? lst-var-expr)
     old-env
     (new-env (cdr lst-var-expr) (add-solo (car lst-var-expr) old-env))))

(define (add-solo  var old-env)
  (cases var-expr var
    (val (iden val-of-iden)
         (extend-env-wrapper iden (value-of val-of-iden old-env) old-env NON-FINAL))
    (final-val (iden val-of-iden)
              (extend-env-wrapper iden (value-of val-of-iden old-env) old-env FINAL))))

;for each different ast node type, e.g. <program>, <expr>, <var-expr> you might
;consider implementing a function with the outline:
#|
(define (value-of-ast-node-type ast)
  (cases ast-node-type ast
    (ast-node-type-variant
     (f1 f2)
     'UNIMPLEMENTED
     )
    (else (raise (~a "value-of-ast-node-type error: unimplemented expression: " ast)))
    )
  )
|#
;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST. Take some time to read it.
;you should be able to understand it by now.
(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec))

;parser is a one argument function that takes a string,
;scans & parses it and generates a resulting abstract
;syntax tree (ast). 
(define parser
  (sllgen:make-string-parser lexical-spec grammar-spec))

;you can use this function to find out more about how
;the string is broken up into tokens during parsing,
;this step is automatically included in the parser
;function. This is a one-argument function that takes a 
;string.
(define scanner
  (sllgen:make-string-scanner lexical-spec grammar-spec))

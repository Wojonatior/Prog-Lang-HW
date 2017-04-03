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
    ;please remove this before you start writting your gramar
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
    )
  )

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
  (value-of (parser program-string))
)

(define (value-of ast)
  (cond
    ((program? ast)
     (value-of-program ast))
    ((expr? ast)
     (value-of-expr ast))
    (else 0)))

(define (value-of-program ast)
  (cases program ast
    (a-program (expr exprs)
      (andmap value-of (flat-list expr exprs)))))

(define (value-of-expr ast)
  (cases expr ast
         (num-expr (num) (eval-num num))
         (down-expr (expr1) (eval-step down-step expr1))
         (up-expr (expr1) (eval-step up-step expr1))
         (left-expr (expr1) (eval-step left-step expr1))
         (right-expr (expr1) (eval-step right-step expr1))
         (point-expr (expr1 expr2) (eval-point expr1 expr2))
         (origin-expr (expr1) (eval-origin expr1))
         (if-expr (cond-expr expr1 expr2) (eval-if cond-expr expr1 expr2))
         (add-expr (expr1 expr2) (eval-add expr1 expr2))
         (move-expr (expr1 expr2 exprs) (eval-move expr1 expr2 exprs))))

(define (eval-num num)
  (num-val num))

(define (eval-step step-type expr1)
  (letrec
    ([val1 (value-of expr1)])
    (step-val (step-type (num-val->n val1)))))

(define (eval-point expr1 expr2)
  (letrec ([val1 (value-of expr1)]
           [val2 (value-of expr2)])
    (point-val
      (point (num-val->n val1) (num-val->n val2)))))

(define (eval-origin expr1)
  (letrec ([cord (point-val->p (value-of expr1))])
    (bool-val
      (and (= 0 (point->x cord))
           (= 0 (point->y cord))))))

(define (eval-if bool-expr expr1 expr2)
  (letrec ([ifbool (value-of bool-expr)]
           [val1 (value-of expr1)]
           [val2 (value-of expr2)])
    (if
      (bool-val->b ifbool)
      val1
      val2)))

(define (eval-move expr1 expr2 exprs)
  (letrec ([start (value-of expr1)])
    (foldl 
      (lambda (arg1 arg2) (move arg1 arg2))
      start
      (cons expr2 exprs))))

(define (move expr1 expr2)
  (letrec
    ([step (step-val->st (value-of expr1))]
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

(define (eval-add expr1 expr2)
 (letrec
     ([l-step-val (value-of expr1)]
      [r-step-val (value-of expr2)]
      [l-step (step-val->st l-step-val)]
      [r-step (step-val->st r-step-val)]
      [res (+ (get-signed-value l-step) (get-signed-value  r-step))])
   (cond
     [(and (valid-steps? l-step r-step) (or (left-step? l-step) (right-step? l-step))) 
      (get-horizontal res)]
     [(and (valid-steps? l-step r-step) (or (up-step? l-step) (down-step? l-step)))
      (get-vertical res)]
     [else (raise "invalid args in add")])))

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

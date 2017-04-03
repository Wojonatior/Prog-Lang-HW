ng racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))
(#%require "hw06-env-values.rkt")

;===============================================================================
;========================= Lexical and Grammar Specs ===========================
;===============================================================================

(define lexical-spec
  '(
    (whitespace (whitespace) skip) ;Ignore whitespace
    (comment ("#" (arbno (not #\newline))) skip) ;Ignore comments, but resume after a newline character
    (number (digit (arbno digit)) number) ;number is composed of one or more digits
    (number ("-" digit (arbno digit)) number) ;number is composed of one or more digits with a negative sign beofre it
    
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol) ;Identifiers start with a letter and then can contain alphanumeric,_,-, or ? characters
    )
  )

(define grammar-spec
  '(
    (program (expr (arbno expr)) a-program)
    
    (expr (number) num-expr) ;Number expressions are prety steight forward, and are typically a base expression
    ;All of the step expressions are relatively straightforward with the type preceeding a set of parens
    (expr ("up" "(" expr ")") up-expr)
    (expr ("down" "(" expr ")") down-expr)
    (expr ("left" "(" expr ")") left-expr)
    (expr ("right" "(" expr ")") right-expr)
    ;Points are composed of two expressions in parens
    (expr ("(" expr expr ")") point-expr)
    
    ;Accept 2 steps on the same axis and return the net step
    (expr ("+" expr expr) add-expr)
    ;Accept a point and returns boolen if at 0,0
    (expr ("origin?" "(" expr ")") origin-expr)
    ;Evaluates boolean in parens, and then evaluates one or other of the expressions after
    (expr ("if" "(" expr ")" "then" expr "else" expr ) if-expr)
    ;Accepts a point, and then one or more steps, returns the new point location
    (expr ("move" "(" expr expr (arbno expr) ")") move-expr)
    
    ;Evaluates the expressions in the block in order and then evaluates to the last expression
    (expr ("{" (arbno var-expr) (arbno expr) "}") block-expr)
    ;Returns the value stored in memory for the identifier
    (expr (identifier) iden-expr)
    ;Stores the expression in the specified identifier
    (var-expr ("val" identifier "=" expr) val)
    ;Same as above, but not allowed to be overwritten
    (var-expr ("final val" identifier "=" expr) final-val)
    )
  )

;given one or more arguments this function will return a flat list
(define (flat-list el1 . rest)
   (flatten (list el1 rest)  )
  )
;===============================================================================
;================================ Value-of =====================================
;===============================================================================
;value-of takes as a parameter an AST resulted from a call to the
;parser function.
(define (run program-string)
  ;Run is what starts the program, which entails starting a chain of value-of calls
  (if (string? program-string)
      (value-of (parser program-string) (empty-env))
      (raise (string-append "expected a program as string, got: " (~a program-string))))) 

(define (value-of ast env)
  (cond 
    ;For ease of reading value-of breaks out into 3 seperate branches, one for each broad category
    [(program? ast) (value-of-program ast env)]
    [(expr? ast) (value-of-expr ast env)]
    [(var-expr? ast) (value-of-var ast env)]
    [else (raise (~a "Unimplemented ast node: " ~a ast))]))

;================================= program =====================================
(define (value-of-program prog env)
  (cases program prog
    (a-program
     (expr rest-of-expressions)
     ;given a non-predicate function, andmap will apply the function
     ;to every element in the list and then return the value of
     ;applying the function on the last element.
     ;-- To run a program entails evaluating ever expr in the "program" and then returning the last value in the list
     (andmap (lambda (ex) (value-of ex env))
             (flat-list expr rest-of-expressions))
     )
    )
  )

;=================================== expr =======================================
(define (value-of-expr ex env)
  ;Since an or will short circuit, this is a quick way to bail without an expression
  (or (expr? ex) (raise (string-append "value-of-expr error: expected an expression, got " (~a ex))))
  (cases expr ex
    (num-expr (n) (num-val n))
    ;Pretty straightforward, extract the number value from the num expression
    
    (up-expr (num)
     ;Evaluate the expression passed as the step quantity
     ;Then extract the number from that
     ;Then create an up step
     ;Then create a step value to return
     (step-val (up-step (num-val->n (value-of num env)))))
    
    (down-expr (num)
     (step-val (down-step (num-val->n (value-of num env)))))
    
    (left-expr (num)
     (step-val (left-step (num-val->n (value-of num env)))))
    
    (right-expr (num)
     (step-val (right-step (num-val->n (value-of num env)))))
    
    ;Identifiers are pretty strightforward, just walking through the env object we have looking for the identifier name
    (iden-expr (var-name)
     (apply-env env var-name))
    
    ;(expr ("[" expr expr "]") point-expr)
    (point-expr (x y)
     ;Evalueate each number expression first and extract their numbers
     ;Then create a point object and return that
     (point-val (point (num-val->n (value-of x env)) (num-val->n (value-of y env))))
     )
    
    ;(expr ("move" "(" expr (arbno expr)")") move-expr)
    ;
    (move-expr (point-expr first-move rest-of-moves)
     (letrec
       ;Get start point
         ([start-p (point-val->p (value-of point-expr env))]
          ;Get all of the moves as expressions
          [all-moves-as-expr (map (lambda (ex) (value-of ex env)) (flat-list first-move rest-of-moves))]
          ;Get the expressed values from all of the expressions
          [all-moves-step (map step-val->st all-moves-as-expr)]
          ;Reduce the step list with the starting point and the simple move function
          [final-p (foldl move start-p all-moves-step)])
       (point-val final-p)
       )
     )
    
    ;
    (add-expr (lhs rhs)
     (letrec
         ([l-step-val (value-of lhs env)]
          [r-step-val (value-of rhs env)]
          [l-step (step-val->st l-step-val)]
          [r-step (step-val->st r-step-val)]
          ;Get the signed sum from adding the two steps
          [res (+ (get-axis-value l-step) (get-axis-value r-step))])
       (cond
         ;If the steps are valid to add and left/right
         ;return
         [(and (valid-steps-for-add? l-step r-step) (or (left-step? l-step) (right-step? l-step))) 
          (get-horizontal-step res) ]

         ;If the steps are valid annd up/down
         ;return
         [(and (valid-steps-for-add? l-step r-step) (or (up-step? l-step) (down-step? l-step)))
          (get-vertical-step res) ]
         [else (raise "invalid args in add")]
         )
       )
     )
    
    ;relativly simple origin checking
    (origin-expr 
     (p-expr)
     (bool-val (equal? (point-val->p (value-of p-expr env)) (point 0 0)))
     )
    
    ;Simple if-then evaluation
    (if-expr 
     (cond then-exp else-exp)
     (let
         ([c-val (bool-val->b (value-of cond env))])
       (if c-val
           (value-of then-exp env)
           (value-of else-exp env))
       )
     )
    
    ;Evaluate the block just like evaluating the program
    ;Start with a new env, but bind the previous one to the new one
    (block-expr
     (lst-of-var-expr lst-of-expr)
     (andmap (lambda (x) (value-of x (build-new-env lst-of-var-expr env)))  lst-of-expr))
    
    (else (raise (~a "value-of-expr error: unimplemented expression: " ex)))
    )
  )

;Returns a point that does simple cartesian addition
(define (move st start-p)
  (cases step st
    (up-step (st)
             (point (point->x start-p) (+ (point->y start-p) st)))
    
    (down-step (st)
               (point (point->x start-p) (- (point->y start-p) st)))
    
    (left-step (st)
               (point ( - (point->x start-p) st) (point->y start-p)))
    
    (right-step (st)
                (point ( + (point->x start-p) st) (point->y start-p)))
    
    )
  )


;========================= helpers for add ================================
(define (valid-steps-for-add? st1 st2)
  (or 
   (and (up-step? st1) (up-step? st2))
   (and (down-step? st1) (down-step? st2))
   (and (up-step? st1) (down-step? st2))
   (and (down-step? st1) (up-step? st2))
   
   (and (left-step? st1) (left-step? st2))
   (and (right-step? st1) (right-step? st2))
   (and (left-step? st1) (right-step? st2))
   (and (right-step? st1) (left-step? st2))
   )
  )

;Add a sign to values as well
(define (get-axis-value st)
  (cases step st
    (up-step (st) st)
    (down-step (st) (* -1 st))
    (left-step (st) (* -1 st))
    (right-step (st) st)
    )
  )

;Add a sign to values for adding 
(define (get-vertical-step num)
  (if (positive? num)
      (step-val (up-step num)) 
      (step-val (down-step (* -1 num)))
      )         
  )

(define (get-horizontal-step num)
  (if (positive? num)
      (step-val (right-step num)) 
      (step-val (left-step (* -1 num)))
      )         
  )

;==========================helpers for block-expr==============
(define (build-new-env lst-var-expr old-env)
 (if (null? lst-var-expr)
     old-env
     (build-new-env (cdr lst-var-expr) (one-at-a-time-add (car lst-var-expr) old-env))))

(define (one-at-a-time-add var old-env)
  (cases var-expr var
    (val (iden val-of-iden)
         (extend-env-wrapper iden (value-of val-of-iden old-env) old-env NON-FINAL))
    (final-val (iden val-of-iden)
              (extend-env-wrapper iden (value-of val-of-iden old-env) old-env FINAL))))

;=================================== var =======================================
(define (value-of-var v-ex env)
  (or (var-expr? v-ex) (invalid-args-exception "value-of-var" "var-expr?" v-ex))
  (cases var-expr v-ex
    (val
     (iden val-of-iden)
     (extend-env-wrapper iden (value-of val-of-iden env) env NON-FINAL))
    
    (final-val
     (iden val-of-iden)
     (extend-env-wrapper iden (value-of val-of-iden env) env FINAL))
    
    (else (raise (~a "value-of-var error: unimplemented expression: " v-ex)))
    )
  )
;===============================================================================
;============================= sllgen boilerplate ==============================
;===============================================================================
;this will create the AST datatype with define-datatype
;according to the lexical and grammar specifications.
(sllgen:make-define-datatypes lexical-spec grammar-spec)

;you can use this function to display the define-datatype
;expression used to generate the AST.
(define (show-data-types)
  (sllgen:list-define-datatypes lexical-spec grammar-spec))

;parser is a one argument function that takes a string,
;scans & parses it and generates a resulting abstract
;syntax tree.
(define parser
  (sllgen:make-string-parser lexical-spec grammar-spec))

;you can use this function to find out more about how
;the string is broken up into tokens during parsing,
;this step is automatically included in the parser
;function. This is a one-argument function that takes a 
;string.
(define scanner
  (sllgen:make-string-scanner lexical-spec grammar-spec))

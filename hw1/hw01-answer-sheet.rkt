#lang racket
(#%provide (all-defined))

#|
If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:
   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the names of any definition
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     but you can change the names of the arguments if you deem it necessary.
   - make sure that you submit an asnwer sheet that compiles! If you cannot write
     a correct solution at least make it compile, if you cannot make it compile then
     comment it out. In the latter case, make sure that the default definitions
     for the problem are still present. Otherwise you may be penalized up to 25%
     of the total points for the homework.
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. You will
lose up to 25% of the total points for the entire homework depending on the number of errors.
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!

|#
;======================================01=======================================
;((3 + 3) * 9)
;equal to 54
(define (p1-1)
  (* 9 (+ 3 3))
)

;((6 * 9) / ((4 + 2) + (4 * 3)))
;equal to 3
;Wrong
(define (p1-2)
  (/ (* 6 9) (+ (+ 4 2) (* 4 3)))
)

;(2* ((20 - (91 / 7)) * (45 - 42)))
;equal to 42
(define (p1-3)
  (* 2 (* (- 20 (/ 91 7)) (- 45 42)))
)
;======================================02=======================================
;write your answer as a string; you do not need to write any special escape
;characters to distinguish new lines.
(define p2
  "Take the most nested expression and move the operator to the beginning of the expression
   Continue doing this with the most nested expression that hasn't been converted until
   the expression is completely converted. "
)
;======================================03=======================================
;;Write the definitions of x,y,z here:
(define x 2)
(define y 3)
(define z 4)

;======================================04=======================================
;you will need to have solved problem 3. The values x,y,z are not parameters
;of this function!
(define (p4)
  (if (= x y z)
    0
    (cond
      ((and (<= z x) (<= z y))
        (+ x y))
      ((and (<= y x) (<= y z))
        (+ x z))
      ((and (<= x y) (<= x z))
        (+ y z))
    )
  )
)
;======================================05=======================================
(define (p5)
  (if (= x y z)
    0
    (cond
      ((and (>= z x) (>= z y))
        (+ x y))
      ((and (>= y x) (>= y z))
        (+ x z))
      ((and (>= x y) (>= x z))
        (+ y z))
      ;Threyyyy
    )
  ) 
)

;======================================06=======================================
(define (p6)
  (= x y)  
)

;======================================07=======================================
;same instructions as problem 02.
(define p7
  "(define thirty-five 35) will result in a variable that contains 35
   (define (thirty-five) 35) will result in a function that returns 35"
)

;======================================08=======================================
;same instructions as problem 02.
(define p8
  "The quote will prevent an expression from being evalutated. For example
    (/ 4 2) would normally evalute to 2, but if quoted it will evaluate to
    `(/ 4 2)"
)

;======================================09=======================================
;same instructions as problem 02.
(define p9
  "quote will not evalutate anything, whereas list will evaluate the variables
   and function calls so you end up with a shallow list"
)

;======================================10=======================================
;same instructions as problem 02.
(define p10
  "(some) Strings are mutable, wheras symbols are not. Symbols can be used essentially
  as an enumeration for some values like 'tall 'medium 'short"
)

;======================================11=======================================
;(4 2 6 9)
(define (p11-1)
  (list `4 `2 `6 `9) 
)

;(spaceship
;  (name(serenity))
;  (class(firefly)))
(define (p11-2)
  (list 'spaceship (list 'name (list 'serenity))(list 'class (list 'firefly))) 
)

;(2 * ((20 - (91 / 7)) * (45 - 42)))
(define (p11-3)
  'UNIMPLEMENTED  
)

;======================================12=======================================
(define example '(a b c))

;(d a b c)
(define (p12-1 lst)
  (cons `d lst)
)

;(a b d a b)
(define (p12-2 lst)
  (cons (car lst) (cons (cadr lst) (cons `d (cons (car lst) (list (cadr lst))))))
)

;(b c d a)
(define (p12-3 lst)
  (cons (cadr lst) (cons (caddr lst) (cons `d (list (car lst)))))
)


;======================================13=======================================
(define p13
  "eq checks for identity, whereas equal checks for equality. Identity is when the
  two variables point at the same object in memory. Equality is when both variables
  contain the same value"
)
; write your answer as a string; you do not need to write any special escape
; characters to distinguish new lines.


;======================================14=======================================
(define (create-error-msg sym val)
  (string-append (string-append "This is a custom error message we will be using next. Symbol '" (symbol->string sym)) (string-append " was not paired with value " (number->string val)))
)
;======================================15=======================================
(define (check-correctness pair)
  (if (eq? `answer-to-everything (car pair))
      (if (equal? 42 (cadr pair))
          #t
          (raise (create-error-msg `answer-to-everything 42)))
      #f)
)

;======================================16=======================================
;No answer necessary - just experiment it as instructed in hw01.txt file


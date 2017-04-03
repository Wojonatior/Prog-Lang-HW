#lang racket
(#%provide (all-defined))
(#%require (lib "eopl.ss" "eopl"))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does not affect the
compilation of the accompanying test file. Changes that are almost certain to break
the above restriction are:
  - changing the names of any definitions that are explicitely used in the tests
    (e.g. function names, relevant constants)

If there are any specific instructions for a problem, please read them carefully. Otherwise,
follow these general rules:

   - replace the 'UNIMPLEMENTED symbol with your solution
   - you are NOT allowed to change the number of arguments of the pre-defined functions,
     because changing the number of arguments automatically changes the semantics of the 
     function. Changing the name of the arguments is permitted since that change only
     affects the readability of the function, not the semantics.
   - you may write any number of helper functions

When done, make sure that the accompanying test file compiles. 
If you cannot come up with a correct solution then please make the answer-sheet
compiles. If you have partial solutions that do not compile please comment them out,
if this is the case, the default definitions will have to be present since the tests
will be expecting functions with the names defined here.

Submission guidelines:
   - please rename this file to hw05-yourlastname.rkt prior to submission
   - also rename hw05-tests.rkt to hw05-yourlastname-tests.rkt
   - upload both hw05-yourlastname.rkt and hw05-tests.rkt
|#
;=======================================01======================================
(define (invalid-args-msg fun-name-as-string
                          expected-value-type-as-predicate-string
                          received)
  (string-append "Invalid arguments in: " fun-name-as-string " --- "
                 "expected: " expected-value-type-as-predicate-string " --- "
                 "received: " (~a received)
                 )
)

;You can compare the contents of this answer sheet with the answer sheet of the
;previous homework to infer what is generated automatically by define-datatype.

(define-datatype step step?
  (up-step (num number?))
  (down-step (num number?))
  (left-step (num number?))
  (right-step (num number?))
  (seq-step (step-1 step?) (step-2 step?)))

(define (up-step? st)
  (if (step? st)
      (cases step st
        (up-step (num) (number? num))
        (down-step (_) #f)
        (left-step (_) #f)
        (right-step (_) #f)
        (seq-step (_ __) #f))
      #f))

(define (down-step? st)
  (if (step? st)
      (cases step st
        (up-step (_) #f)
        (down-step (num) (number? num))
        (left-step (_) #f)
        (right-step (_) #f)
        (seq-step (_ __) #f))
      #f))


(define (left-step? st)
  (if (step? st)
      (cases step st
        (up-step (_) #f)
        (down-step (_) #f)
        (left-step (num) (number? num))
        (right-step (_) #f)
        (seq-step (_ __) #f))
      #f))


(define (right-step? st)
  (if (step? st)
      (cases step st
        (up-step (_) #f)
        (down-step (_) #f)
        (left-step (_) #f)
        (right-step (num) (number? num))
        (seq-step (_ __) #f))
      #f))


(define (seq-step? st)
  (if (step? st)
      (cases step st
        (up-step (_) #f)
        (down-step (_) #f)
        (left-step (_) #f)
        (right-step (_) #f)
        (seq-step (step-1 step-2) (and (step? step-1) (step? step-2))))
      #f))


;;to avoid needless duplication we will only implement one extractor to handle all the
;;simple steps, rather than 4. So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (step? st)
    (cases step st
        (up-step (num) num)
        (down-step (num) num)
        (left-step (num) num)
        (right-step (num) num)
        (seq-step (step-1 step-2) (invalid-args-msg "single-step->n" "not seq-step?" st) ))
    (invalid-args-msg "single-step->n" "single-step?" st)))

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (if (step? st)
    (cases step st
        (up-step (num) (invalid-args-msg "seq-step->st-1" "seq-step?" st))
        (down-step (num) (invalid-args-msg "seq-step->st-1" "seq-step?" st))
        (left-step (num) (invalid-args-msg "seq-step->st-1" "seq-step?" st))
        (right-step (num) (invalid-args-msg "seq-step->st-1" "seq-step?" st))
        (seq-step (step-1 step-2) step-1))
    (invalid-args-msg "seq-step->st-1" "step?" st)))


(define (seq-step->st-2 st)
  (if (step? st)
    (cases step st
        (up-step (num) (invalid-args-msg "seq-step->st-2" "seq-step?" st))
        (down-step (num) (invalid-args-msg "seq-step->st-2" "seq-step?" st))
        (left-step (num) (invalid-args-msg "seq-step->st-2" "seq-step?" st))
        (right-step (num) (invalid-args-msg "seq-step->st-2" "seq-step?" st))
        (seq-step (step-1 step-2) step-2))
    (invalid-args-msg "seq-step->st-2" "step?" st)))

;;===================================
(define (move start-p st)
  (define x (first start-p))
  (define y (second start-p))
  (if (step? st)
    (cases step st
        (up-step (num) (list x (+ y num)))
        (down-step (num) (list x (- y num)))
        (left-step (num) (list (- x num) y))
        (right-step (num) (list (+ x num) y))
        (seq-step (step-1 step-2) (move (move start-p step-1) step-2)))
    (invalid-args-msg "move" "step?" st)))

;=======================================02======================================
;2.a
(define (exception-no-binding-msg sym)
  (string-append "No binding for '" (~a sym))
  )

;
(define-datatype environment environment?
    (empty-env)
    (extend-env (var symbol?)(val number?)(environment environment?))
    (extend-env-final (var symbol?)(val number?)(environment environment?))
  )

(define (apply-env env search-sym)
  (cases environment env
    (empty-env () (exception-no-binding-msg search-sym))
    (extend-env-final (saved-sym saved-val saved-env)
        (if (eq? search-sym saved-sym)
            saved-val
            (apply-env saved-env search-sym)))
    (extend-env (saved-sym saved-val saved-env)
        (if (eq? search-sym saved-sym)
            saved-val
            (apply-env saved-env search-sym)))))

;==========
;2.b
(define (exception-sym-final-msg sym)
  (string-append "Symbol '" (~a sym) " is final and cannot be overriden.")
  )

;It is prefered to give meaningfull names to marker values.
;In the tests we will be using these two values to invoke
;the extend-env-wrapper function
(define FINAL #t)
(define NON-FINAL #f)


(define (extend-env-wrapper sym val old-env final?)
  (if (is-final? old-env sym)
      (exception-sym-final-msg sym)
      (if final?
          (extend-env-final sym val old-env)
          (extend-env sym val old-env))))

(define (is-final? env search-sym)
  (cases environment env
    (empty-env () #f)
    (extend-env (_ __ saved-env)
        (is-final? saved-env search-sym))
    (extend-env-final (saved-sym _ saved-env)
        (if (eq? search-sym saved-sym)
            #t
            (is-final? saved-env search-sym)))))
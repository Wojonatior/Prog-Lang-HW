#lang racket
(#%provide (all-defined))

#|
IMPORTANT:
Overall, you are allowed to change this file in any way that does *not* affect the
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
   - please rename the file to hw04-yourlastname-answer.rkt prior to submission
   - only the renamed file file needs to be uploaded
|#
;======================================01=======================================
(define-syntax-rule (for {var <- value-range} yield result)
  (map (lambda (value) (let ((var value)) result)) value-range))
;======================================02=======================================
(define-syntax-rule (seq expr1 expr2)
  ((lambda () expr1 expr2)))
  ;(map (lambda (expression) expression) `(expr1 expr2)))

;====
(define-syntax-rule (while condition body)
  (let ((loop_cond condition))
    (if (loop_cond)
        (lambda () body '(while condition body))
        0)))


;======================================03=======================================
#|
<step> ::=  <step>  <step>       "seq-step"
          | "up" number          "up-step"
          | "down" number        "down-step"
          | "left" number        "left-step"
          | "right" number       "right-step"
|#
;example of how to create the error message for the "up-step" constructor
;> (invalid-args-msg "up-step" "number?" '(1 2 3 4))
;where '(1 2 3 4) should be replaced by the actual violating value.

;;you can reorder the functions below if it better suits your needs
;Constructors
(define (up-step n)
  (if (number? n)
      (list 'up n)
      (raise (string-append "Invalid arguments in: up-step --- expected: number? --- received: " n))))


(define (down-step n)
  (if (number? n)
      (list 'down n)
      (raise (string-append "Invalid arguments in: down-step --- expected: number? --- received: " n))))

(define (left-step n)
  (if (number? n)
      (list 'left n)
      (raise (string-append "Invalid arguments in: left-step --- expected: number? --- received: " n))))

(define (right-step n)
  (if (number? n)
      (list 'right n)
      (raise (string-append "Invalid arguments in: right-step --- expected: number? --- received: " n))))

(define (seq-step st-1 st-2)
  (if (step? st-1)
      (if (step? st-2)
          (list 'seq st-1 st-2)
          (raise (string-append "Invalid arguments in: seq-step --- expected: step? --- received: " st-2)))
      (raise (string-append "Invalid arguments in: seq-step --- expected: step? --- received: " st-1))))

;;====
;Predicates
(define (up-step? st)
  (if (and (list? st) (= (length st) 2))
      (and (eq? 'up (car st)) (number? (second st)))
      #f))

(define (down-step? st)
  (if (and (list? st) (= (length st) 2))
      (and (eq? 'down (car st)) (number? (second st)))
      #f))

(define (left-step? st)
  (if (and (list? st) (= (length st) 2))
      (and (eq? 'left (car st)) (number? (second st)))
      #f))

(define (right-step? st)
  (if (and (list? st) (= (length st) 2))
      (and (eq? 'right (car st)) (number? (second st)))
      #f))
(define (seq-step? st)
  (if (and (list? st) (= (length st) 3))
      (and (eq? 'seq (car st)) (step? (second st)) (step? (third st)))
      #f))

;This is a predicate that tells you whether or not something is a step,
;it should return true when given either up, down, left, right or seq steps.
(define (step? st)
  (if (list? st)
      (or (eq? (car st) 'right) (eq? (car st) 'left) (eq? (car st) 'up) (eq? (car st) 'down) (eq? (car st) 'seq))
          #f))


;; to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. 
;; So this should take: up, down, left and right steps.
(define (single-step->n st)
  (if (and (list? st) (= (length st) 2))
      (second st)
      (raise (string-append "Invalid arguments in: single-step->n --- expected: single-step? --- received: " st))))
      

;;two extractors, one for each piece of data representing a sequential step
(define (seq-step->st-1 st)
  (second st))


(define (seq-step->st-2 st)
  (third st)) 

;;===================================
(define (move start-p step)
  (let ((x (first start-p))(y (second start-p)))
  (cond
    [(eq? (car step) 'right)
     (list (+ x (single-step->n step)) y)]
    [(eq? (car step) 'left)
     (list (- x (single-step->n step)) y)]
    [(eq? (car step) 'up)
     (list x (+ y (single-step->n step)))]
    [(eq? (car step) 'down)
     (list x (- y (single-step->n step)))]
    [(eq? (car step) 'seq)
     (move (move start-p (seq-step->st-1 step)) (seq-step->st-2 step))]
    )))


;======================================04=======================================
;singleton-set should return a function that takes a number as an argument and
;tells whether or not that number is in the set
(define (singleton-set x)
  (lambda (num) (equal? x num)))


;the set of all elements that are in either 's1' or 's2'
(define (union s1 s2)
  (lambda (num) (or (s1 num) (s2 num))))

;the set of all elements that are in both  in 's1' and 's2'
(define (intersection s1 s2)
  (lambda (num) (and (s1 num) (s2 num))))

;the set of all elements that are in 's1', but that are not in 's2'
(define (diff s1 s2)
  (lambda (num) (and (s1 num) (not (s2 num)))))

;returns the subset of s, for which the predicate 'predicate' is true.
(define (filter predicate s)
  (lambda (num) (and (predicate num) (s num))))

;we assume that the sets can contain only numbers between 0 and bound
(define bound 100)

;returns whether or not the set contains at least an element for which
;the predicate is true. s below is the parameter standing for a given set
(define (exists? predicate s)
  (ormap (lambda (x) (and (s x) (predicate x))) (range bound))
  )

;returns whether or not the predicate is true for all the elements
;of the given set s
(define (all? predicate s)
  (andmap (lambda (x) (if (s x) (predicate x) #t)) (range bound))
  )

;returns a new set where "op" has been applied to all elements
; NOTE: just because a procedure/function has the word "map" in it, it 
;       doesn't mean you have to use map higher order function to implement it. 
;       Map is a functional operation with well defined behavior that 
;       is not tied to any implementation.
(define (map-set op s)
  'UNIMPLEMENTED
  )

;just a sample predicate
(define (prime? n)
  (define (non-divisible? n)
    (lambda (i)
      (not (= (modulo n i) 0))))
  (define range-of-prime-divisors (cddr (range (+ (integer-sqrt n) 1))))
  (if (equal? n 1)
      #f
      (andmap (non-divisible? n) range-of-prime-divisors)
      )
  )

;=====================================05====================================
; FYI:
;  to emphasize the procedural-based approach to implement "step" data type and to
;  contrast it with the data structure-based approach for "step" implementation 
;  used in p3, here we add "-proc" suffix to each corresponding function name.

;====p5-a================
(define (up-step-proc n)
  'UNIMPLEMENTED  
)

(define (down-step-proc n)
  'UNIMPLEMENTED  
)

(define (left-step-proc n)
  'UNIMPLEMENTED  
)

(define (right-step-proc n)
  'UNIMPLEMENTED  
)

(define (seq-step-proc st-1 st-2)
  'UNIMPLEMENTED  
)

;;====
(define (up-step-proc? st)
  'UNIMPLEMENTED  
)

(define (down-step-proc? st)
  'UNIMPLEMENTED  
)

(define (left-step-proc? st)
  'UNIMPLEMENTED  
)

(define (right-step-proc? st)
  'UNIMPLEMENTED  
)

(define (seq-step-proc? st)
  'UNIMPLEMENTED  
)

;This is a predicate that tells you whether or not st is a step,
; it should return true when given either up, down, left, right or seq steps.
(define (step-proc? st)
  'UNIMPLEMENTED
)


;;to avoid needless duplication we will only implement one extractor to handle all the
;; simple steps, rather than four of them. So this should take: up, down, left and right 
;; steps. 
(define (single-step-proc->n st)
  'UNIMPLMENTED  
)

;;two extractors
(define (seq-step-proc->st-1 st)
  'UNIMPLEMENTED  
)


(define (seq-step-proc->st-2 st)
  'UNIMPLEMENTED  
)
;;========p5-b
(define (move-proc start-p step-proc)
  'UNIMPLEMENTED  
)
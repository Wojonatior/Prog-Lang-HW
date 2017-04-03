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
   - you may write any number of helper functions as you want.

When done, make sure that the accompanying test file compiles. 
|#
;======================================01=======================================
(define (foldl-335 op default-el lst)
   (if (empty? lst)
       default-el
       (foldl-335 op (op (car lst) default-el) (cdr lst)))
)

;---
(define (foldr-335 op default-el lst)
   (foldl-335 op default-el (reverse lst))
)

;======================================02=======================================
(define (andmap-335 test-op lst)
  ;This is a boolean algebra solution I also came up with
  ;(not (ormap (lambda (x) (not (test-op x))) lst))
  (if (= (length lst) 0)
      #t
      (if (test-op (car lst))
          (andmap-335 test-op (cdr lst))
           #f)))

;======================================03=======================================
(define (filter-335 test-op lst)
  (filter-helper test-op lst `())
)

(define (filter-helper test-op remaining filtered)
  (if (empty? remaining)
      filtered
      (if (test-op (car remaining))
          (filter-helper test-op (cdr remaining) (append filtered (list (car remaining))))
          (filter-helper test-op (cdr remaining) filtered))))

;======================================04=======================================
(define (map-reduce m-op r-op default-el lst)
  (foldl r-op default-el (map m-op lst))
)

;======================================05=======================================
(define (series n)
  (map-reduce calc-value + 0 (range (+ n 1))))

(define (calc-value n)
  (/ (expt -1 n) (! (+ n 1))))

(define (! n)
  (if (= n 1)
      1
      (* n (! (- n 1)))))
;======================================06=======================================
(define (zip lst1 lst2)
  (if (= 0 (length lst1))
      '()
      (cons (cons (car lst1) (list (car lst2))) (zip (cdr lst1) (cdr lst2))))
)

;======================================07=======================================
(define (matrix-to-vector op mat)
  ;Get a new list that is MxN instead of NxM
  ;Reduce each element in that list to 1 value (use the first/last element as the default for the list
  (map (lambda (lst) (smart-foldl op lst)) (transpose mat))
)
;Takes a MxN matrix and returns a NxM matrix
;I know this solution is kinda gross
(define (transpose mat)
  (map flatten (smart-foldl zip mat)))

;folds that use the first element as the default
(define (smart-foldl op lst)
  (if (empty? lst)
      `()
      (foldl op (car lst) (cdr lst))))

(define (smart-foldr op lst)
  (smart-foldl op (reverse lst)))
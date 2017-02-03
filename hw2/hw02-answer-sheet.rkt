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
     for the problem are still present. 
   - you may use any number of helper functions you deem necessary.

When done, make sure that you do not get any errors when you hit the "Run" button. 
If you cannot come up with a correct solution then please make the answer-sheet
compile correctly and comment any partial solution you might have; if this is the case,
the default definitions will have to be present!
|#
;======================================01=======================================
(define (list-of-even-numbers? lst)
  (if (list? lst)
  (if (empty? lst)
      #t
      (if (number? (car lst))
           (if (even? (car lst))
               (list-of-even-numbers? (cdr lst))
               #f)
           #f)
      )
  #f)
)

;======================================02=======================================
;;for n > 0
;Sn = 1/1 + 1/4 + 1/9 + 1/16 + ...
(define (series-a n)
  (if (> n 0)
      (+ (/ 1 (* n n)) (series-a (- n 1)))
      0
  )
)

;====
;;for n >= 0
;Sn = 1 - 1/2 + 1/6 - 1/24 + ...
(define (series-b n)
  (if (= n 0)
      1
      (+ (/ (expt -1 n) (! (+ n 1))) (series-b (- n 1)))
   )
)

(define (! n)
  (if (= n 1)
      1
      (* n (! (- n 1)))
  )
)

;======================================03=======================================
(define (carpet n)
  (if (= n 0)
      '((%))
      (add-top-bom      
       (expand-each-line (carpet (- n 1)) (decide-sym n))
       n) ))

(define (decide-sym n)
  (if (odd? n)
      '+
      '%))

(define (expand-each-line carp sym) ;sym=> %, or +
  (if (null? carp)
      '()
       (cons (append  (cons sym (car carp) ) (list sym)) ; expanded first line
        (expand-each-line (cdr carp) sym)))  ; expanded the rest of the carpet except for the first line
  )

(define (add-top-bom carp n)
  (append (cons (compose-top n) carp)
          (list (compose-top n))))

(define (compose-top n)
  (compose-helper (decide-sym n) n)
)

(define (compose-helper sym n)
  (if (= n 0)
      (list sym)
      (cons sym (cons sym (compose-helper sym (- n 1))))
  )
)


;======================================04=======================================
(define (pascal n)
  (if (= n 1)
      `((1))
      (insert (new-last-row (last (pascal (- n 1))))
              (pascal (- n 1)))))

(define (new-last-row old-last-row)
  (new-last-row-endings (new-last-row-core old-last-row)))
  
(define (new-last-row-core old-last-row) ;Builds core of new row given last row
  (if (= 1 (length old-last-row))
      `()
      (cons (+ (second old-last-row) (first old-last-row))
            (new-last-row-core (cdr old-last-row)))))

(define (new-last-row-endings core)
  (append (cons 1 core) `(1)))

(define (insert line pascal)
  (append pascal (list line)))
;======================================05=======================================
(define (balanced? in)
  (paren-counter (string->list in) 0)
)

 (define (paren-counter in counter)
   (if (= counter -1)
       #f
       (if (= (length in) 0) ;If the list is empty
           (if (= counter 0) 
               #t ;And the counter = 0, then it's balanced
               #f )
           (if (equal? (car in) "(")
               (paren-counter (cdr in) (+ counter 1)) ;Increment counter if opening
               (if (equal? (car in) ")")
                   (paren-counter (cdr in) (- counter 1)) ;Decrement counter if closing
                   (paren-counter (cdr in) counter)))))) ;Do nothing if it's a different char

;======================================06=======================================
(define (list-of-all? predicate lst)
  (if (= (length lst) 0)
      #t
      (and (predicate (car lst))
           (list-of-all? predicate (cdr lst))))
)

;======================================07=======================================
(define (create-mapping keys vals)
  (if (= (length keys) (length vals))
      (if (list-of-all? symbol? keys)
          (lambda(sym)
            (key-finder sym keys vals))
          (raise "The keys are not all symbols."))
      (raise "The lists are not of equal length."))
)
  
(define (key-finder key keys vals)
  (if (= 0 (length keys))
      (raise (string-append "Could not find mapping for symbol '" (symbol->string key)))
      (if (eq? key (car keys))
          (car vals)
          (key-finder key (cdr keys) (cdr vals))))
)
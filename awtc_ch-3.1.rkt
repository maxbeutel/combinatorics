#lang racket

(require rackunit)

(define (flatmap proc L)
  (foldr (lambda (i tmp)
           (append tmp (proc i))) '() L))

(define (permutations L)
  (if (null? L)
      '(())
      (flatmap (lambda (i)
                 (map (lambda (p) (cons i p))
                      (permutations (remove i L)))) L)))

(define (sequences length from)
  (if (= length 1)
      (map (lambda (i) (list i)) from)
      (flatmap (lambda (i)
                 (map (lambda (seq) (cons i seq))
                      (sequences (- length 1) from))) from)))

;;;;;;;;;;;;;;

(check-equal? (permutations '()) '(()))

(check-equal? (permutations '(1 2 3)) '((3 2 1) (3 1 2) (2 3 1) (2 1 3) (1 3 2) (1 2 3)))

;;;;;;;;;;; Quick Check, p. 46

;; (1)
;; How many ways are there to permute the elements of the set [7] so that an even number is in the first position?

(define perms (permutations (list 1 2 3 4 5 6 7)))

(define first-digit-is-even (filter (lambda (p) (even? (car p))) perms))

(check-equal? (length first-digit-is-even) 2160) ;; the answer

;; (2)
;; How many ways are there to permute elements of the multiset
;; {1, 1, 2, 2, 3, 4, 5, 6} so that the first and last elements are different?

(define non-uniq (permutations '(1 1 2 2 3 4 5 6)))

(define total-uniq (set-count (apply set non-uniq)))

(define first-digit-equals-last-digit (filter (lambda (p) (= (car p) (last p))) non-uniq))

(check-equal? (length first-digit-equals-last-digit) 2880) ;; here we are still double-counting!

(check-equal? total-uniq 10080) ;; unique count of all possible permutations

(define first-digit-equals-last-uniq (set-count (apply set first-digit-equals-last-digit)))

(check-equal? first-digit-equals-last-uniq 720) ;; unique count

(check-equal? (- total-uniq first-digit-equals-last-uniq) 9360) ;; the answer

;;;;;;;;;;; Quick Check, p. 50

(check-equal? (sequences 1 '(1 2 3)) '((1) (2) (3)))

(check-equal? (sequences 2 '(1 2 3)) '((3 1) (3 2) (3 3) (2 1) (2 2) (2 3) (1 1) (1 2) (1 3)))

(define six-digit-pos-int (filter (lambda (n) (> (car n) 0)) (sequences 6 '(1 2 3 4 5 6 7 8 9 0))  ))

(check-equal? (length six-digit-pos-int) 900000)

;; (1)
;; How many six-digit positive integers are there in which the first and last digits are the same?

(define six-digit-first-last-same (filter (lambda (n) (= (car n) (last n))) six-digit-pos-int))

(check-equal? (length six-digit-first-last-same) 90000) ;; the answer

;; (2)
;; How many six-digit positive integers are there in which the first and last digits are of the same parity?

(define parity-filter (lambda (n)
                        (or
                         (and (even? (car n)) (even? (last n)))
                         (and (odd? (car n)) (odd? (last n))))))

(define six-digit-first-last-same-parity (filter parity-filter six-digit-pos-int))

(check-equal? (length six-digit-first-last-same-parity) 450000) ;; the answer

#lang racket

;; In this file we use bruteforce to validate answers to the "Quick Check" questions in Miklos Bona's
;; book "A Walk Through Combinatorics, 4th edition", Chapter 3.
;;
;; As there are no answers given in the book, I thought it would be an interesting task to validate
;; my answers by writing some racket programs.

(require rackunit)

;; Utility functions

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

(check-equal? (permutations '()) '(()))

(check-equal? (permutations '(1 2 3)) '((3 2 1) (3 1 2) (2 3 1) (2 1 3) (1 3 2) (1 2 3)))

;;;;;;;;;;; Quick Check, p. 46

;; (1)
;; How many ways are there to permute the elements of the set [7] so that an even number is in the first position?

(define perms (permutations (list 1 2 3 4 5 6 7)))

(define first-digit-is-even (filter (lambda (p) (even? (car p))) perms))

(check-equal? (length first-digit-is-even) 2160) ;; The answer

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

(check-equal? (- total-uniq first-digit-equals-last-uniq) 9360) ;; The answer

;;;;;;;;;;; Quick Check, p. 50

(check-equal? (sequences 1 '(1 2 3)) '((1) (2) (3)))

(check-equal? (sequences 2 '(1 2 3)) '((3 1) (3 2) (3 3) (2 1) (2 2) (2 3) (1 1) (1 2) (1 3)))

(define six-digit-pos-int (filter (lambda (n) (> (car n) 0)) (sequences 6 '(1 2 3 4 5 6 7 8 9 0))  ))

(check-equal? (length six-digit-pos-int) 900000)

;; (1)
;; How many six-digit positive integers are there in which the first and last digits are the same?

(define six-digit-first-last-same (filter (lambda (n) (= (car n) (last n))) six-digit-pos-int))

(check-equal? (length six-digit-first-last-same) 90000) ;; The answer

;; (2)
;; How many six-digit positive integers are there in which the first and last digits are of the same parity?

(define parity-filter (lambda (n)
                        (or
                         (and (even? (car n)) (even? (last n)))
                         (and (odd? (car n)) (odd? (last n))))))

(define six-digit-first-last-same-parity (filter parity-filter six-digit-pos-int))

(check-equal? (length six-digit-first-last-same-parity) 450000) ;; The answer

;;;;;;;;;;;;;;;;;;;;;

;; (3)
;; How many functions f : [n] -> [n] are there for which there exists exactly one i element of [n] satisfying f(i) = i?

(define (count-self-mapped-items func)
  (define (iter current-func to)
    (cond ((null? current-func) 0)
          ((= (car current-func) to) (+ 1 (iter (cdr current-func) (+ 1 to))))
          (else (+ 0 (iter (cdr current-func) (+ 1 to))))))
  (iter func 1))

(check-equal? (count-self-mapped-items '(1 2 3)) 3)

(check-equal? (count-self-mapped-items '(3 1 2)) 0)

(check-equal? (count-self-mapped-items '(1 1 1)) 1)

(define all-funcs (sequences 5 '(1 2 3 4 5)))

(check-equal? (length (filter (lambda (f) (= (count-self-mapped-items f) 1)) all-funcs)) 1280) ;; The answer

;;;;;;;;;;; Quick Check, p. 53

;; (1)
;; A company has 20 male and 15 female employees.
;; How many ways are there to form a committee consisting of four male and three female employees of the company?

(define num-M 2) ;; Number of men in the committee.
(define num-F 2) ;; Number of women in the committee.

;; i <= 20, then M, > 20 F, total 35.
(define employees '( 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
                    21 22 23 24 25 26 27 28 29 30 31 32 33 34 35))

(define (is-M? id) (<= id 20))

(define (is-F? id) (> id 20))

(define (permutations-k L k)
  (if (= k 0) ;; base case: n P 0 = 1, the empty permutation
      '(())
      (flatmap (lambda (i)
             (map (lambda (p) (cons i p))
                  (permutations-k (remove i L) (- k 1)))) L)))

(define committee-perms (permutations-k employees (+ num-M num-F))) ;; Pick a committee of k from all employees.

(define committee-combs (apply set (map (lambda (p) (sort p <)) committee-perms))) ;; But order doesn't matter, so uniqify

;; Now find all committees that consist of num-M men and num-F women.
(check-equal? (length (filter (lambda (c) (and (= (length (filter is-M? c)) num-M) (= (length (filter is-F? c)) num-F)))
                (set->list committee-combs))) 19950) ;; The answer for 2 M and 2 F.

;; Generally, the answer should be equal to:
;;     20 C num-M * 20 C num-F
;; For the question in the book, the answer is 2204475, but that's too slow to find via brute-force,
;; could be optimized using memoization.

;; (2) and (3) are somewhat obvious, for (2) if we assume 40 hours/5 days, we can pick 3 hours out of 40 to be office hours.
;; That is 40 C 3. For (3), it's the combinations formula vs. the multiset formula. 10 C 5 vs. 10 multi-C 4, because in the
;; second lottery repetitions are possible, that is, we need to count the possible multi-sets.

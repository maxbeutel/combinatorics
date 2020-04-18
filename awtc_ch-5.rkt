#lang racket

;; Assumption n >= k
(define (compositions n k)
  (cond ((= n 0) '())
        ((= k 1) (list (list n)))
        (else (append (map (lambda (C) (append C '(1))) ;; add a '1' at the end
                           (compositions (- n 1) (- k 1)))
                      (map (lambda (C) (list-set C (- (length C) 1)  (+ (last C) 1))) ;; increment the last element by 1
                           (compositions (- n 1) k))))))

;; Assumption n >= k
(define (weak-compositions n k)
  (cond
    ((= n 0) (list (build-list k (lambda (x) 0))))
    ((= k 1) (list (list n)))
    (else
     (append
      (map (lambda (C) (list-set C (- (length C) 1)  (+ (last C) 1))) ;; increment the last element by 1
           (weak-compositions (- n 1) k))
      (map (lambda (C) (append C '(0))) ;; add a '0' at the end
           (weak-compositions n (- k 1)))))))

;; Utility
(define (flatmap proc L)
  (foldr append '() (map proc L)))

;; Just playing around:
;; Subsets vs. permutations
(define (subsets S)
  (if (null? S)
      '(())
      (let ((tmp (subsets (cdr S))))
        (append tmp
                (map (lambda (sub) (cons (car S) sub))
                     tmp)))))

(define (permutations S)
  (if (null? S)
      '(())
      (flatmap (lambda (i)
             (map (lambda (perm) (cons i perm))
                  (permutations (remove i S)))) S)))

;;;;;;;;;;;;;

(length (weak-compositions 4 2))

(length (weak-compositions 5 3))

;; Playing around with listing all compositions, an answer to Chen's question
;; https://devblogs.microsoft.com/oldnewthing/20140714-00/?p=513
;; Question:
;; If you wanted to generate all compositions of any length,
;; you could do it by generating all compositions of length 1,
;; then compositions of length 2, and so on up to length n.
;; What’s the easier way of doing it?
;; Easier would be to generate all subsets of (n-1)

(define n 4)

(define all-subsets
  (subsets (build-list (- n 1) (lambda (x) (+ x 1)))))

(define all-compositions
  (flatmap (lambda (i) (compositions n i))
           (build-list n (lambda (x) (+ x 1)))))

all-subsets
(length all-subsets)

all-compositions
(length all-compositions)

;; But what's the bijection?

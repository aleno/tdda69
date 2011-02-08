
;;; --------------------------------------------------------------------------
;;;  higher-order.ss
;;;  Auxiliary functions for the first assignments in the course "Data and
;;;  Program Structures" (TDDA69).
;;; --------------------------------------------------------------------------

;; The identity function

(define (id x) x)

;; The square funtion

(define (square x) (* x x))

;; A function for generating increment functions

(define (addx x) (lambda (y) (+ y x)))

;; Testing for primality (SICP Section 1.2.6)

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (and (> n 1)
       (= n (smallest-divisor n))))

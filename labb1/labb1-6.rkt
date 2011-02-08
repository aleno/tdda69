(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))))

(define (filter-accumulate filter combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (if (filter a) (term a) null-value)
                (filter-accumulate filter combiner null-value term (next a) next b))))

(define (add-even a b)
  (filter-accumulate (lambda (x) (even? x)) + 0 (lambda (x) x) a (lambda (x) (+ x 1)) b))


(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

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

(define (sum-prime-squares a b)
  (filter-accumulate prime? + 0 square a 1+ b))

(define (mult-relative-primes n)
  (filter-accumulate (lambda (x) (= (gcd x n) 1)) * 1 id 1 1+ n))

(define (repeated f n)
  (lambda (x)
    (define (repeat a)
      (f (if (= a 1) x
             (repeat (1- a)))))
    (repeat n)))

(define (combine f g)
  (lambda (x) (f (g x))))

(define (repeated2 f n)
  (accumulate combine f (lambda (x) f) 2 1+ n))

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x 0.001)) (f x) (f (+ x 0.001))) 3)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
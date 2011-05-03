(load "higher-order.ss")
;; Uppgift 6: Ytterligare generalisering

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

;; deluppgift b
(define (sum-prime-squares a b)
  (filter-accumulate prime? + 0 square a 1+ b))

;; deluppgift c
(define (mult-relative-primes n)
  (filter-accumulate (lambda (x) (= (gcd x n) 1)) * 1 id 1 1+ n))

;; Uppgift 7: Högre ordningens funktioner igen

(define (repeated f n)
  (lambda (x)
    (define (repeat a)
      (f (if (= a 1) x
             (repeat (1- a)))))
    (repeat n)))

;; deluppgift b

(define (combine f g)
  (lambda (x) (f (g x))))

(define (repeated2 f n)
  (accumulate combine f (lambda (x) f) 2 1+ n))

;; Uppgift 8: Högre ordningens funktioner för sista gången

(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x 0.001)) (f x) (f (+ x 0.001))) 3)))

;; deluppgift b

(define (n-fold-smooth f n)
  ((repeated smooth n) f))
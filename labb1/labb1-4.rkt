;; Uppgift 4: Högre ordningens funktioner och rationella tal

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (1+ n)
  (+ n 1))
  
(define (fakultet n)
  (product (lambda (x) (* x 1)) 1 1+ n))

(define (pi2-term n)
  (if (even? n)
      (/ n (1+ n))
      (/ (1+ n) n)))

(define (pi2 n)
  (exact->inexact (* 4 (product pi2-term 2 1+ n))))
;; Uppgift 5: Generalisering

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))
(define (product term a next b)
  (accumulate * 1 term a next b))

;; Test
(accumulate * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 4)
(accumulate-iter * 1 (lambda (x) x) 1 (lambda (x) (+ x 1)) 4)

;(accumulate expt 2 (lambda (x) x) 2 (lambda (x) (+ x 1)) 4) -> mycket stort.
;(accumulate-iter expt 2 (lambda (x) x) 2 (lambda (x) (+ x 1)) 4) -> 16777216

;; Alla funktioner som kan operera på två argument. Detta gör att vi kan köra
;; den med matematiska operationer som + - * / expt men inte sin cos tan
;; eftersom de endast tar ett argument. Genom detta kan vi köra accumulate med
;; cons vilket kommer sätta ihop värdena till en lista.

;; T.ex. Om vi skickar cons kommer innehållet i listan bli i omvänd ordning då
;; den rekursiva processlösningen consar på vägen tillbaka medan den iterativa
;; processlösningen är färdig när slutvillkoret nås.

(load "/home/TDDA69/Lab/r6rs/constraints.ss")

(define (squarer a square)
  (define (process-new-value)
    (cond ((has-value? a)
           (if (> (get-value a) 0)
               (set-value! square (* (get-value a) (get-value a)) me)
               (error 'squarer
                      "square less than 0: ~s" (get-value a))))
          ))
  (define (process-forget-value)
    (forget-value! square me)
    (forget-value! a me)
    )
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error 'squarer "Unknown request: ~s" request)))
    )
  (connect a me)
  (connect square me)
  me)

(define a (make-connector))
(define sq (make-connector))

(squarer a sq)

(probe "A" a)
(probe "SQ" sq)

(set-value! a 5 'user)
(forget-value! a 'user)
(set-value! sq -3 'user)

;; Problemet som kan uppstå om man implementerar den genom
;; (define (squarer a b)
;;   (multiplier a a b)) ;; (define (multiplier u v w)..

;; Är att när man ändrar a så kommer multiplier att notera att dess argument u har ändrats och
;; signalerar då ändringar till v, w. v är kopplat till u vilket kommer ge ytterliggare ett genomslag
;; vilket inte blir så bra.
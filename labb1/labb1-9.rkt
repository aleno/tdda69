;;; --------------------------------------------------------------------------
;;;  account.ss
;;;  Template for the bank account assignment in the course "Data and
;;;  Program Structures" (TDDA69).
;;; --------------------------------------------------------------------------

(define (make-account balance time interestrate)
  (define (withdraw amount now)
    (if (>= now time)
        (begin (set! balance (+ balance (* (- now time) interestrate balance)))
               (set! time now)
               (if (>= balance amount)
                   (begin (set! balance (- balance amount))
                          balance)
                   "Insufficient funds"))
        "Too late"))
  (define (deposit amount now)
    (if (>= now time)
        (begin (set! balance (+ balance (* (- now time) interestrate balance)))
               (set! time now)
               (set! balance (+ balance amount))
               balance)
        "Too late"))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error 'dispatch "Unknown request: ~s" m))))
  dispatch)

(define my-account (make-account 1000 100 0.08))
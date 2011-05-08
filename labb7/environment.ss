
;;; --------------------------------------------------------------------------
;;;  environment.ss
;;;  Environment handling for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;;
;;; 2009-11-25 Uppaterad till version 6 (mutable cons). /AH --------------------------------------------------------------------------

;;; Creating environments
;;; ---------------------

;; An environment is a list of frames where the first frame is the
;; car of the list and the enclosing environment is the cdr. The
;; empty environment is represented as the empty list.

;; Each frame is a cons pair where the car is a list of symbols and
;; the cdr is a list of corresponding values.

(define the-empty-environment '())

(define (enclosing-environment env) 
  ;; environment -> environment
  (cdr env))

(define (first-frame env) 
  ;; environment -> frame
  (car env))

(define (make-frame variables values)
  ;; list of variables x list of values -> frame
  (mcons variables values))

(define (frame-variables frame)
  ;; frame -> list of variables
  (mcar frame))

(define (frame-values frame) 
  ;; frame -> list of values
  (mcdr frame))

;; Extending environments
;; ----------------------

(define (add-binding-to-frame! var val frame)
  ;; variable x value x frame ->
  (set-mcar! frame (mcons var (mcar frame)))
  (set-mcdr! frame (mcons val (mcdr frame))))

;; (define (extend-environment vars vals base-env)
;;   ;; list of variables x list of values x environment -> environment
;;   (if (= (length vars) (length vals))
;;       (cons (make-frame vars vals) base-env)
;;       (error 'extend-environment
;;         (if (< (length vars) (length vals))
;;             "Too many arguments supplied: ~s ~s"
;;             "Too few arguments supplied: ~s ~s")
;;         vars vals)))

;; In normal Scheme we can define functions that accept an arbitrary
;; number of argument by using a dotted argument list, e.g.:
;;
;;   (define (count . args)
;;      (length args))
;;
;; We want to be able to do this in %Scheme as well, and for this reason
;; we have changed the implementation of extend-environment to accept
;; dotted variable lists.

(define (extend-environment vars vals base-env)
  ;; list of variables x list of values x environment -> environment
  (define (make-frame-local invars invals outvars outvals)
    (cond ((and (null? invars) (null? invals))
           (make-frame outvars outvals))
          ((null? invars)
           (error 'extend-environment "Too many arguments supplied"))
          ((null? invals)
           (error 'extend-environment "Too few arguments supplied"))
          ((atom? invars)
           (make-frame-local (cons invars '())
                             (cons invals '())
                             outvars outvals))
          (else (make-frame-local (cdr invars)
                                  (cdr invals)
                                  (mcons (car invars) outvars)
                                  (mcons (car invals) outvals)))))
  (cons (make-frame-local vars vals '() '()) base-env))

;;; Finding values in environments
;;; ------------------------------

(define (lookup-variable-value var env)
  ;; variable x environment -> value
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (mcar vals))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error 'lookup-variable-value "Unbound variable: ~s" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

;;; Modifying environments
;;; ----------------------

(define (set-variable-value! var val env)
  ;; variable x value x environment ->
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (if (eq? env the-empty-environment)
        (error 'set-variable-value! "Unbound variable: ~s" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  ;; variable x value x environment ->
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (mcar vars))
             (set-mcar! vals val))
            (else (scan (mcdr vars) (mcdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

;;; The initial environment for %scheme
;;; -----------------------------------

;; The %scheme environment is stored in the variable the-global-environment.
;; Initially this contains the pre-defined symbols %true and %false, and
;; a number of primitive procedures. Each primitive procedure, e.g. %car,
;; is bound to a list ($primitive car) where the first element is an
;; indicator and the second element the name of the original scheme procedure
;; to call.

(define primitive-procedures
  (list (list '%car car)
        (list '%cdr cdr)
        (list '%cons cons)
        (list '%null? null?)
        (list '%list list)
        (list '%eq? eq?)
        (list '%+ +)
        (list '%- -)
        (list '%* *)
        (list '%/ /)
        (list '%= =)
        (list '%< <)
        (list '%> >)
        (list '%newline newline)
        (list '%display display)))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list '$primitive (cadr proc)))
       primitive-procedures))

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! '%true #t initial-env)
    (define-variable! '%false #f initial-env)
    initial-env))

(define the-global-environment (setup-environment))

;;; --------------------------------------------------------------------------

(display "Loaded environment.ss")
(newline)

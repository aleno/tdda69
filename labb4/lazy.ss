
;;; --------------------------------------------------------------------------
;;;  lazy.ss
;;;  Lazy evaluator for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).

;;; Modifierad för Scheme version 6 (mutable cons). Thunk-objekten.
;; / AH 2009-11-25
;;; --------------------------------------------------------------------------
;; (load "TDDA69/Lab/meta_eval.ss")
(load "/home/TDDA69/Lab/r6rs/meta_eval.ss")
;; (load "meta_eval.ss")

;; Utökning uppgift 2 för parameteröverföringsmodeller.

(define (procedure-parameters p)
  (map (lambda (x)
         (if (pair? x) (car x) x)) (cadr p)))

(define (procedure-parameters-model p)
  (map (lambda (x)
         (if (pair? x) (cadr x) '%normal)) (cadr p)))


;;; New core of the evaluator
;;; -------------------------

(define (eval-%scheme exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-%scheme (cond->if exp) env))
        ((application? exp)
         (apply-%scheme (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error 'eval-%scheme "Unknown expression type ~s" exp))))

(define (actual-value exp env)
  (force-it (eval-%scheme exp env)))

(define (apply-%scheme procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args ;; Ändrad.
            (procedure-parameters-model procedure)
            arguments env)
           (procedure-environment procedure))))
        (else
         (error 'apply-%scheme "Unknown procedure type ~s" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

;; We can easily switch between different parameter passing methods by
;; setting the value of the variable parameter-passing-method to either
;; call-by-name or call-by-need.

(define parameter-passing-method 'call-by-name)
;(define parameter-passing-method 'call-by-need)

;(define (list-of-delayed-args exps params env)
;  (cond ((no-operands? exps)
;	 '())
;	((eq? parameter-passing-method 'call-by-name)
;	 (cons (delay-it (first-operand exps) env)
;	       (list-of-delayed-args (rest-operands exps) (cdr params) env)))
;	((eq? parameter-passing-method 'call-by-need)
;	 (cons (delay-it-memo (first-operand exps) env)  
;	       (list-of-delayed-args (rest-operands exps) (cdr params) env)))
;	(else
;	 (error 'list-of-delayed-args "Invalid parameter passing method."))))

;; Uppgift 2
(define (list-of-delayed-args props exps env)
  (define (delay-it prop exp)
    (cond ((eq? prop '%normal)
           (actual-value exp env))
          ((eq? prop '%lazy)
           (delay-it exp env))
          ((eq? prop '%lazy-memo)
           (delay-it-memo exp env))
          (else
           (error 'list-of-delayed-args "Invalid parameter passing method."))))
  (map delay-it props exps))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval-%scheme (if-consequent exp) env)
      (eval-%scheme (if-alternative exp) env)))

;;; Representing thunks
;;; -------------------

;; To facilitate transformation between call-by-name and call-by-need the
;; implementation knows about two types of thunks: normal thunks (for 
;; call-by-name) and memo-thunks (for call-by-need). The latter ones are
;; transformed into evaluated-thunks when evaluated the first time.

(define thunk '$thunk)           

(define memo-thunk '$memo-thunk) 

(define evaluated-thunk '$evaluated-thunk)

(define (delay-it exp env)
  (mcons thunk (mcons exp (mcons env '()))))

(define (delay-it-memo exp env)
  (mcons memo-thunk (mcons exp (mcons env '()))))

(define (thunk? obj)
  (tagged-list? obj thunk))

(define (memo-thunk? obj)
  (tagged-list? obj memo-thunk))

(define (thunk-exp thunk) (mcar (mcdr thunk)))

(define (thunk-env thunk) (mcar (mcdr (mcdr thunk))))

(define (evaluated-thunk? obj)
  (tagged-list? obj evaluated-thunk))

(define (thunk-value evaluated-thunk) (mcar (mcdr evaluated-thunk)))

(define (force-it obj)
  (cond ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-mcar! obj evaluated-thunk)
           (set-mcar! (mcdr obj) result)  ; replace exp with its value
           (set-mcdr! (mcdr obj) '())     ; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;;; New user interface
;;; ------------------

(define (driver-loop)
  (newline)
  (display "%==> ")
  (do ((%exp (read)(read)))
      ((memq %exp '(exit stop logout hej-d�)) 
       (display "Have a nice day!") (newline))
      (user-print (actual-value %exp the-global-environment))
      (newline)
      (display "%==> ")))

(define (user-print object)
  (define (circular-print object)
    (cond ((or (not (list? object)) (eq? object '())) object)
	  ((compound-procedure? object)  
	   (list '<$procedure
		 (procedure-parameters object)
		 (procedure-body object)
		 '<procedure-environment>>))
	  ((primitive-procedure? object)
	   (list '<$primitive>))
	  ((thunk? object)
	   (list '<$thunk (thunk-exp object) '<thunk-environment>>))
	  ((memo-thunk? object)
	   (list '<$memo-thunk (thunk-exp object) '<thunk-environment>>))
	  (else (cons (circular-print (car object))
		      (circular-print (cdr object))))))
  (display (circular-print object)))


;; Test av strömmar för uppgift 1.
;(remote-eval '(%define (%cons a (b %lazy-memo)) (%lambda (x) (x a b))))
;(remote-eval '(%define (%car x) (x (%lambda (a b) a))))
;(remote-eval '(%define (%cdr x) (x (%lambda (a b) b))))

;; För uppgift 3 så har vi gjort följande ändringar.
;(%define (%stream-car stream) (stream (%lambda (a b) a)))
;(%define (%stream-cdr stream) (stream (%lambda (a b) b)))

;; Definera cons-stream som %lazy-memo eftersom %lazy är så mycket segare,
;; på grund av att strömen måste beräknas om varje gång i primtals grejen.

;(%define (%cons-stream a (b %lazy-memo))
;         (%lambda (x) (x a b)))

;; Testa ström operationer.
(load "streams.%ss")

;; Körexempel för uppgift 2..

(remote-eval '(%define (f1 a b) (%if (%= a 0) 1 b)))
(remote-eval '(%define (f2 a (b %lazy)) (%if (%= a 0) 1 b)))
(remote-eval '(%define (f3 a (b %lazy-memo)) (%if (%= a 0) 1 b)))

;(remote-eval '(f1 1 (%/ 1 0))) ;; -> division by zero
;(remote-eval '(f2 1 (%/ 1 0))) ;; -> thunk
;(remote-eval '(f3 1 (%/ 1 0))) ;; -> memo-thunk

;;; --------------------------------------------------------------------------

(display "Loaded lazy.ss")
(newline)

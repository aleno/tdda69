
;;; --------------------------------------------------------------------------
;;;  meta_eval.ss
;;;  Evaluator for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------

;; (load "TDDA69/Lab/abssyntax.ss")
;; (load "TDDA69/Lab/environment.ss")

(load "abssyntax.ss")
(load "environment.ss")

;; (load "abssyntax.ss") 
;; (load "environment.ss")

(define (delay? exp)
  (tagged-list? exp '%delay))
(define (force? exp)
  (tagged-list? exp '%force))

;(define (promise? exp) (tagged-list? exp '%promise))
(define (cons-stream? exp) 
  (tagged-list? exp '%cons-stream))

(define (defmacro? exp)
  (tagged-list? exp '%defmacro))

(define (macro? exp)
  (tagged-list? exp '$macro))

;;; Core of the evaluator
;;; ---------------------

(define (eval-%scheme exp env)
  ;(print "Eval[")
 ; (print exp)
 ; (print "]...")
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
        ((dolist? exp) ;; Utökning uppgift 3
         (eval-dolist exp env))
        ((delay? exp) ;; Utökning uppgift 3
         (eval-delay exp env))
        ((force? exp)
         (eval-force exp env))
        ((cons-stream? exp)
         (eval-cons-stream exp env))
        ((defmacro? exp)
         (eval-defmacro exp env))
        ((macro? exp)
         (eval-macro exp env))
        ((application? exp)
         (expand-and-apply exp env))
        (else
         (error 'eval-%scheme "Unknown expression type: ~s" exp))))

(define (expand-and-apply exp env)
  ; Hämta procedure
  (let ((proc (eval-%scheme (operator exp) env)))
    ; Om proc är ett macro expandera och kör!
    (if (macro? proc)
        (eval-%scheme (apply-%scheme
                       (cdr proc)
                       (operands exp)) env)
        ; Annars vanlig procedur. :)
        (apply-%scheme proc (list-of-values (operands exp) env)))))

(define (apply-%scheme procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
         (error 'apply-%scheme "Unknown procedure type: ~s" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval-%scheme (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval-%scheme (if-predicate exp) env))
      (eval-%scheme (if-consequent exp) env)
      (eval-%scheme (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval-%scheme (first-exp exps) env))
        (else (eval-%scheme (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval-%scheme (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval-%scheme (definition-value exp) env)
                    env)
  'ok)

;;; Representing procedure objects
;;; ------------------------------

(define (make-procedure parameters body env)
  (list '$procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p '$procedure))

(define (procedure-parameters p) (cadr p))

(define (procedure-body p) (caddr p))

(define (procedure-environment p) (cadddr p))

(define (primitive-procedure? proc)
  (tagged-list? proc '$primitive))

(define (primitive-implementation proc) (cadr proc))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

;;; User interface
;;; --------------

(define (driver-loop)
  (newline)
  (display "%==> ")
  (do ((%exp (read)(read)))
      ((memq %exp '(exit stop logout hej-d�)) 
       (display "Have a nice day!") (newline))
      (user-print (eval-%scheme %exp the-global-environment))
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
	  (else (cons (circular-print (car object))
		      (circular-print (cdr object))))))
  (display (circular-print object)))

;; (init-%scheme) initializes the global environment, and calls driver-loop.
;; To start %Scheme without initializing, call (go-%scheme).

(define (init-%scheme)
  (set! the-global-environment (setup-environment))
  (driver-loop))

(define (go-%scheme)
  (driver-loop))

;; Useful abbreviations

(define init init-%scheme)
(define go go-%scheme)

;; (remote-eval exp) makes it possible to evaluate %scheme-expressions from
;; Chez Scheme. The main use of this is to make definitions in the global
;; environment, without entering the interactive driver-loop.

(define (remote-eval %-exp)
  (user-print (eval-%scheme %-exp the-global-environment))
  (newline))

;;; Examples
;;; --------

;; Here are some examples that you may want to try out:

;; (remote-eval '(%define (fact n) (%if (%= 1 n) 1 (%* n (fact (%- n 1))))))
;; (remote-eval '(fact 10)

;; Utökning för uppgift 2  
(remote-eval '(%define (%map la li) 
                       (%if (%null? li) 
                            (%list) 
                            (%cons (la (%car li)) (%map la (%cdr li))))))

(remote-eval '(%map %1+ (%list 1 2 3 4))) 
(remote-eval '(%map (%lambda (x) (%* 2 x)) (%list 1 2 3 4))) 

;; Utökning för uppgift 3
(define (eval-dolist exp env)
  (define (loop varlist)
    (if (null? varlist)
        null
        (begin
          (eval-sequence (dolist-actions exp)
                         (extend-environment
                          (car (dolist-args exp))
                          (car varlist)
                          env))
          (loop (cdr varlist)))))
  (loop (eval-%scheme (cadr (dolist-args exp)) env))
  (lookup-variable-value (caddr (dolist-args exp)) env))

(remote-eval '(%define (%sumlist 1st)
                       (%define sum 0)
                       (%dolist (x 1st sum) (%set! sum (%+ sum x)))))
(remote-eval '(%sumlist (%quote (1 2 3))))

(define (eval-delay exp env)
  (cons '%promise 
        (make-procedure '()
                        (list (cadr exp))
                        env)))

(define (eval-force exp env)
  (let ((eval (eval-%scheme (cadr exp) env)))
    (if (tagged-list? eval '%promise)
        (apply-%scheme (cdr eval) '())
        eval)))

(define (eval-cons-stream exp env)
  (let ((a (cadr exp))
        (b (caddr exp)))
    (eval-%scheme `(%cons ,a (%delay ,b)) env)))

(remote-eval '(%define %stream-null? %null?))
(remote-eval '(%define (%the-empty-stream?) (%quote)))
(remote-eval '(%define (%stream-car stream) (%car stream)))
(remote-eval '(%define (%stream-cdr stream) (%force (%cdr stream))))

;(load "streams.%ss")


;; Labb 3b upg 5
(define (eval-defmacro exp env)
  (define-variable! (definition-variable exp)
    (cons '$macro (eval-%scheme (assignment-value exp) env))
    env)
  'ok)

(define (eval-macro exp env)
  (print (cdr exp))
  (newline)
         
  (eval-%scheme (cdr exp) env))
(remote-eval '(%defmacro %setq! (%lambda (var value) (%list (%quote %set!) var (%list (%quote %quote) value)))))
(remote-eval '(%define a 2))
(remote-eval '(%setq! a (+ 2 5)))
(remote-eval 'a)

(remote-eval '(%define arb (%lambda args args)))
;(define (eval-macro exp env))

(remote-eval '(%defmacro %let (%lambda (varlist expr)
                                    (%cons (%list (%quote %lambda)
                                                  (%map %car varlist)
                                                  expr)
                                                  (%map %cadr varlist)))))

(remote-eval '(%let ((a 3)
      (b 4)
      (c 5))
  (%* a b c)))

;;; --------------------------------------------------------------------------

(display "Loaded meta_eval.ss")
(newline)


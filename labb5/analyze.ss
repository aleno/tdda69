
;;; --------------------------------------------------------------------------
;;;  analyze.ss
;;;  Evaluator for the %Scheme language that separates analysis from execution
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------

;; (load "TDDA69/Lab/meta_eval.ss")
;; (load "TDDA69/Lab/r6rs/meta_eval.ss")
(load "meta_eval.ss")

;;; New core of the evaluator
;;; -------------------------

(define (eval-%scheme exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
	((dolist? exp) (analyze-dolist exp))
        ((application? exp) (analyze-application exp))
        (else
         (error 'analyze "Unknown expression type." exp))))

;;; Creating execution procedures
;;; -----------------------------

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error 'analyze-sequence
	       "Empty sequence."))
    (loop (car procs) (cdr procs))))

(define (analyze-dolist exp)
  (let ((varargs (cadr exp))
        (list (analyze (cadr (cadr exp))))
        (res (analyze (caddr (cadr exp))))
        (exps (analyze-sequence (cdr (cdr exp)))))
    (lambda (env)
      (define (loop-dolist varlist)
        (if (null? varlist)
            (res env)
            (begin
              (exps (extend-environment (car varargs)
                                        (car varlist)
                                        env))
              (loop-dolist (cdr varlist)))))
      (loop-dolist (list env)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error 'execute-application
		"Unknown procedure type." ))))

;;; Extended abstract syntax
;;; ------------------------

(define (dolist? exp) 
  (tagged-list? exp '%dolist))

;;; --------------------------------------------------------------------------

(display "Loaded analyze.ss")
(newline)
(remote-eval '(%define sum 0))
(remote-eval '(%define (%sumlist 1st)
                       (%dolist (x 1st sum) (%set! sum (%+ sum x)))))
(remote-eval '(%sumlist (%quote (1 2 3))))
(display "value is")
(newline)
(remote-eval 'sum)
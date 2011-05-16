
;;; --------------------------------------------------------------------------
;;;  ec_eval.ss
;;;  Register machine for %Scheme interpretation
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------
;; (load "TDDA69/Lab/meta_eval.ss")
;; (load "/home/TDDA69/Lab/r6rs/meta_eval.ss")
(load "meta_eval.ss")

;; (load "TDDA69/Lab/regsim.ss")
;; (load "/home/TDDA69/Lab/r6rs/regsim.ss")
(load "regsim.ss")

;;; --------------------------------------------------------------------------
;;;  Auxiliary abstract syntax
;;; --------------------------------------------------------------------------

;; The global environment

(define (get-global-environment)
  the-global-environment)

;; Help functions for implementing tail-recursion

(define (empty-arglist) '())

(define (adjoin-arg arg arglist)
  (append arglist (list arg)))

(define (last-operand? ops)
  (null? (cdr ops)))

(define (no-more-exps? seq) (null? seq))

;; Interfacing compiled and interpreted code

(define (make-compiled-procedure entry env)
  (list '$compiled-procedure entry env))

(define (compiled-procedure? proc)
  (tagged-list? proc '$compiled-procedure))

(define (compiled-procedure-entry c-proc) (cadr c-proc))

(define (compiled-procedure-env c-proc) (caddr c-proc))

;; Printing the result (overrides previous versions)

(define (user-print object)
  (cond ((compound-procedure? object)
	 (display (list '$procedure
			(procedure-parameters object)
			(procedure-body object)
			'<procedure-env>)))
        ((compiled-procedure? object)
         (display '<compiled-procedure>))
	(else (display object))))

;;; --------------------------------------------------------------------------
;;;  Operations that are borrowed from Scheme
;;; --------------------------------------------------------------------------

;; In order for the register machine to work we borrow quite a lot of
;; functions from Scheme, mostly the abstract syntax handling and the
;; environment handling. We could implement these routines as well, using
;; the list structures from section 5.3, but that would to some extent
;; obscure the overall point of using the register machine: to demonstrate
;; how to implement a low-level interpreter.

(define eceval-operations
  (list

   ;; primitive Scheme operations
   (list 'read read)
   (list 'display display)
   (list 'newline newline)
   (list 'memq memq)

   ;; used by compiled code
   (list 'list list)
   (list 'cons cons)
   (list 'null? null?)
   (list 'car car)
   (list 'cdr cdr)

   ;; operations for abstract syntax
   (list 'self-evaluating? self-evaluating?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   (list 'variable? variable?)
   (list 'assignment? assignment?)
   (list 'assignment-variable assignment-variable)
   (list 'assignment-value assignment-value)
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   (list 'cond? cond?)
   (list 'cond->if cond->if)
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'last-exp? last-exp?)
   (list 'first-exp first-exp)
   (list 'rest-exps rest-exps)
   (list 'application? application?)
   (list 'operator operator)
   (list 'operands operands)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)

   ;; operations for dealing with procedures etc
   (list 'true? true?)
   (list 'false? false?)
   (list 'make-procedure make-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-body procedure-body)
   (list 'procedure-environment procedure-environment)

   ;; operations for environment handling
   (list 'extend-environment extend-environment)
   (list 'lookup-variable-value lookup-variable-value)
   (list 'set-variable-value! set-variable-value!)
   (list 'define-variable! define-variable!)
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'user-print user-print)
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   (list 'no-more-exps? no-more-exps?)	
   (list 'get-global-environment get-global-environment)

   ;; for compiled code
   (list 'make-compiled-procedure make-compiled-procedure)
   (list 'apply apply)
   (list 'compiled-procedure? compiled-procedure?)
   (list 'compiled-procedure-entry compiled-procedure-entry)
   (list 'compiled-procedure-env compiled-procedure-env)
   
   ))

;;; --------------------------------------------------------------------------
;;;  The machine itself
;;; --------------------------------------------------------------------------

(define eceval
  (make-machine
   '(exp env val proc argl continue unev
	 compapp) ;*for compiled to call interpreted
   eceval-operations
  '(

;;*for compiled to call interpreted (from exercise 5.47)
  (assign compapp (label compound-apply))
;;*next instruction supports entry from compiler (from section 5.5.7)
  (branch (label external-entry))
read-eval-print-loop
  (perform (op initialize-stack))
  (perform (op newline))
  (perform (op display) (const "%==> "))
  (assign exp (op read))
  (test (op memq)(reg exp)(const (exit stop logout hej-d�)))
  (branch (label done))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (label eval-dispatch))
print-result
  (perform (op print-statistics))
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

;;*support for entry from compiler (from section 5.5.7)
external-entry
  (perform (op initialize-stack))
  (assign env (op get-global-environment))
  (assign continue (label print-result))
  (goto (reg val))

unknown-expression-type
  (assign val (const unknown-expression-type-error))
  (goto (label signal-error))

unknown-procedure-type
  (restore continue)
  (assign val (const unknown-procedure-type-error))
  (goto (label signal-error))

signal-error
  (perform (op user-print) (reg val))
  (goto (label read-eval-print-loop))

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op cond?) (reg exp))
  (branch (label ev-cond))
  (test (op lambda?) (reg exp))
  (branch (label ev-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))
ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))
ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))
ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure)
              (reg unev) (reg exp) (reg env))
  (goto (reg continue))

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign argl (op empty-arglist))
  (assign proc (reg val))
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc)
ev-appl-operand-loop
  (save argl)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label ev-appl-accumulate-arg))
  (goto (label eval-dispatch))
ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))
ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))
ev-appl-accum-last-arg
  (restore argl)
  (assign argl (op adjoin-arg) (reg val) (reg argl))
  (restore proc)
  (goto (label apply-dispatch))
apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proc))  
  (branch (label compound-apply))
;;*next added to call compiled code from evaluator (section 5.5.7)
  (test (op compiled-procedure?) (reg proc))  
  (branch (label compiled-apply))
  (goto (label unknown-procedure-type))

;;*next added to call compiled code from evaluator (section 5.5.7)
compiled-apply
  (restore continue)
  (assign val (op compiled-procedure-entry) (reg proc))
  (goto (reg val))

primitive-apply
  (assign val (op apply-primitive-procedure)
              (reg proc)
              (reg argl))
  (restore continue)
  (goto (reg continue))

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
              (reg unev) (reg argl) (reg env))
  (assign unev (op procedure-body) (reg proc))
  (goto (label ev-sequence))

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

ev-sequence
;  (assign exp (op first-exp) (reg unev))
;  (test (op last-exp?) (reg unev))
;  (branch (label ev-sequence-last-exp))
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))
  
ev-sequence-end
  (restore continue)
  (goto (reg continue))

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (op if-predicate) (reg exp))
  (goto (label eval-dispatch))
ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))
ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))
ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))

ev-cond
  (assign exp (op cond->if) (reg exp))
  (goto (label eval-dispatch))
  
ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))
  (goto (label eval-dispatch))
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))

ev-definition
  (assign unev (op definition-variable) (reg exp))
  (save unev)
  (assign exp (op definition-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-definition-1))
  (goto (label eval-dispatch))
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (op define-variable!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
done
   )))

;;; --------------------------------------------------------------------------
;;;  Startup
;;; --------------------------------------------------------------------------

;; (init-%scheme) initializes the global environment, and starts the machine.
;; To start %scheme without initializing, call (go-%scheme).

(define (init-%scheme)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag #f)
  (start eceval))

(define (go-%scheme)
  (set-register-contents! eceval 'flag #f)
  (start eceval))

(define init init-%scheme)

(define go go-%scheme)

;;; --------------------------------------------------------------------------

(display "Loaded ec_eval.ss")
(newline)



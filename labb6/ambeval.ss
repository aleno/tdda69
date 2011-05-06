
;;; --------------------------------------------------------------------------
;;;  ambeval.ss
;;;  Non-deterministic evaluator for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------

(load "meta_eval.ss")

;;; --------------------------------------------------------------------------
;;;  The non-deterministic evaluator
;;; --------------------------------------------------------------------------

;;; Core of the non-deterministic evaluator
;;; ---------------------------------------

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (if-fail? exp)
  (tagged-list? exp '%if-fail))

(define (permanent-assignment? exp)
  (tagged-list? exp '%permanent-set!))

(define (dolist? exp)
  (tagged-list? exp '%dolist))

(define (analyze exp)
  (cond ((self-evaluating? exp) 
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((dolist? exp) (analyze-dolist exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ; **
        ((amb? exp) (analyze-amb exp))                ; **
        ((application? exp) (analyze-application exp))
        (else
         (error 'analyze "Unknown expression type: ~s" exp))))

;;; Simple expressions
;;; ------------------

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

;;; Conditionals and sequences
;;; --------------------------

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                   (cproc env succeed fail2)
                   (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

;;Uppgift 2

(define (analyze-if-fail exp)
  (let ((pproc (analyze (cadr exp)))
        (cproc (analyze (caddr exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (val next-alternative)
               val)
             (lambda ()
               (cproc env succeed fail))))))
             
(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error 'analyze-sequence "Empty sequence"))
    (loop (car procs) (cdr procs))))

;;; Uppgift 3: %dolist
;;; ------------------

(define (analyze-dolist exp)
  (let ((varargs (cadr exp))
        (list (analyze (cadr (cadr exp))))
        (res (analyze (caddr (cadr exp))))
        (exps (analyze-sequence (cdr (cdr exp)))))
    (lambda (env succeed fail)
      (define (loop-dolist varlist fail)
        (if (null? varlist)
            (res env succeed fail)
            (begin
              (display "x = ")
              (display (car varlist))
              (newline)
              (exps (extend-environment (car varargs)
                                        (car varlist)
                                        env)
                    (lambda (var fail2)
                      (loop-dolist (cdr varlist) fail2))
                    fail))))
      (list env (lambda (value fail2)
                  (loop-dolist value fail2))
            (lambda ()
              (display "I've failed...")
              (fail))
            
            ))))

;;; Definitions and assignments
;;; ---------------------------

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env                        
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
               (let ((old-value
                      (lookup-variable-value var env))) 
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()    ; *2*
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

;; Uppgift 2

(define (analyze-permanent-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)        ; *1*
                 (set-variable-value! var val env)
                 (succeed 'ok
                          fail2))
             fail))))

;;; Procedure applications
;;; ----------------------

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                            proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
      (succeed '() fail)
      ((car aprocs) env
                    ;; success continuation for this aproc
                    (lambda (arg fail2)
                      (get-args (cdr aprocs)
                                env
                                ;; success continuation for recursive
                                ;; call to get-args
                                (lambda (args fail3)
                                  (succeed (cons arg args)
                                           fail3))
                                fail2))
                    fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
         (error 'execute-application "Unknown procedure type: ~s" proc))))

;;; Amb expressions
;;; ---------------

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
            (fail)
            ((car choices) env
                           succeed
                           (lambda ()
                             (try-next (cdr choices))))))
      (try-next cprocs))))

;;; --------------------------------------------------------------------------
;;;  Extensions
;;; --------------------------------------------------------------------------

;;; Extended abstract syntax
;;; ------------------------

(define (amb? exp) (tagged-list? exp 'amb))

(define (amb-choices exp) (cdr exp))

;;; Support for let (as noted in footnote 56, p.428)
;;; ------------------------------------------------

(define (let? exp) (tagged-list? exp '%let))

(define (let-bindings exp) (cadr exp))

(define (let-body exp) (cddr exp))

(define (let-var binding) (car binding))

(define (let-val binding) (cadr binding))

(define (make-combination operator operands) (cons operator operands))

(define (let->combination exp)
  (let ((bindings (let-bindings exp)))
    (make-combination (make-lambda (map let-var bindings)
                                   (let-body exp))
                      (map let-val bindings))))
               
;;; Extended list of primitives
;;; ---------------------------

;; For some examples and exercises we need to inherit some more
;; primitives from Scheme.

(set! primitive-procedures
      (append primitive-procedures
	      (list (list '%memq memq)
		    (list '%member member)
		    (list '%not not)
		    (list '%<= <=)
		    (list '%>= >=)
		    (list '%mod modulo)
		    (list '%and (lambda (x y) (and x y)))
		    (list '%or (lambda (x y) (or x y)))
		    (list '%abs abs)
		    (list '%remainder remainder)
		    (list '%integer? integer?)
		    (list '%sqrt sqrt)
		    (list '%max max)
		    (list '%min min))))

;;; --------------------------------------------------------------------------
;;;  User interface
;;; --------------------------------------------------------------------------

;;; New driver loop
;;; ---------------

(define input-prompt "[amb-eval] %==> ")

(define output-prompt "[amb-eval] value = ")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of ")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string))

(define (announce-output string)
  (newline) (display string))

;;; New startup
;;; -----------

(define (init-amb)
  (set! the-global-environment (setup-environment))
  (driver-loop))

(define (go-amb)
  (driver-loop))

(define (remote-eval exp)
  (ambeval exp
	   the-global-environment
	   (lambda (val next-alternative)
	     val)
	   (lambda ()
	     'fail)))
   
;;; --------------------------------------------------------------------------
(load "amb-functions.%ss")

(display "Loaded ambeval.ss")
(newline)

(remote-eval '(%let ((a (amb 1 2 3 4 5))) (%require (%= (%mod a 2) 0)) a))
(remote-eval '(%begin 
               (%define a (amb 1 2 3 4 5))
               (%require (%= (%mod a 2) 0))
               a))

;; Uppgift 1
(remote-eval '(%define (foo) (%let ((a1 1)
                     (a2 14)
                     (a3 14)
                     (a4 4)
                     
                     (b2 (amb 6 7 8 9 10))
                     (b3 (amb 6 7 8 9 10))
                     (c2 (amb 6 7 8 9 10))
                     (c3 (amb 6 7 8 9 10))
                     
                     (b1 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15))
                     (c1 (amb 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)))
                    (%require (%= (%+ b2 b3 c2 c3) 33))
                    (%let ((b4 (%- 33 b1 b2 b3))
                           (c4 (%- 33 c1 c2 c3))
                           (d1 (%- 33 a1 b1 c1))
                           (d2 (%- 33 a2 b2 c2))
                           (d3 (%- 33 a3 b3 c3)))
                          (%require (%and (%> b4 0) (%< b4 16)))
                          (%require (%and (%> c4 0) (%< c4 16)))
                          (%require (%and (%> d1 0) (%< d1 16)))
                          (%require (%and (%> d2 0) (%< d2 16)))
                          (%require (%and (%> d3 0) (%< d3 16)))
                          (%let ((d4 (%- 33 d1 d2 d3)))
                                (%require (%and (%> d4 0) (%< d4 16)))
                                (%require (%= 33 (%+ a1 b2 c3 d4)))
                                (%require (%= 33 (%+ a4 b3 c2 d1)))
                                (%display (%list a1 a2 a3 a4))
                                (%display (%list b1 b2 b3 b4))
                                (%display (%list c1 c2 c3 c4))
                                (%display (%list d1 d2 d3 d4))
                                )))))

;; Uppgift 2

(remote-eval '(%begin
               (%define count 0)
               (%let ((x (%an-element-of (%quote (a b c))))
                      (y (%an-element-of (%quote (a b c)))))
                     (%permanent-set! count (%+ count 1))
                     (%require (%not (%eq? x y)))
                     (%list x y count))))
  
(remote-eval '(%if-fail (%let ((x (%an-element-of (%quote (1 3 5)))))
                              (%require (%= 0 (%mod x 2)))
                              x)
                        (%quote all-odd)))

(remote-eval '(%if-fail (%let ((x (%an-element-of (%quote (1 3 5 8)))))
                              (%require (%= 0 (%mod x 2)))
                              x)
                        (%quote all-odd)))


;; Uppgift 3: dolist

(remote-eval '(%begin
               (%define sum 0)
               (%define count 0)
               (%dolist (x (%list 2 4 (amb 5 6)) (%display sum))
                                  (%permanent-set! count (%+ count 1))
                                  (%require (%= (%mod x 2) 0))
                                  (%set! sum (%+ sum x)))
               (%display count)))

(remote-eval '(%begin
               (%define count 0)
               (%dolist (x (amb (%list 2 4 5) (%list 2 4 6))
                           (%display sum))
                        (%permanent-set! count (%+ count 1))
                        (%require (%= (%mod x 2) 0))
                        (%set! sum (%+ sum x)))
               (%display count)))
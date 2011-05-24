(load "compiler.ss")

(define (dolist? exp)
  (tagged-list? exp '%dolist))

(define (compile% exp target linkage c-t-env)
  (cond ((self-evaluating? exp)
         (compile-self-evaluating exp target linkage c-t-env))
        ((quoted? exp) (compile-quoted exp target linkage c-t-env))
        ((variable? exp)
         (compile-variable exp target linkage c-t-env))
        ((assignment? exp)
         (compile-assignment exp target linkage c-t-env))
        ((definition? exp)
         (compile-definition exp target linkage c-t-env))
        ((if? exp) (compile-if exp target linkage c-t-env))
        ((lambda? exp) (compile-lambda exp target linkage c-t-env))
        ((begin? exp)
         (compile-sequence (begin-actions exp)
                           target
                           linkage
			   c-t-env))
        ((cond? exp) (compile% (cond->if exp) target linkage c-t-env))
        ((dolist? exp) (compile-dolist exp target linkage c-t-env))
        ((application? exp)
         (compile-application exp target linkage c-t-env))
        (else
         (error 'compile "Unknown expression type: ~s" exp))))

(define (compile-dolist exp target linkage c-t-env)
  #f
  )

(define (dolist-body exp)
  (cddr exp))

(define (dolist-var exp)
  (caadr exp))
(define (dolist-args exp)
  (cadadr exp))
(define (dolist-term-exp exp)
  (car (cddadr exp)))

(define (compile-dolist-loop exp target linkage c-t-env)
  (preserving
   '(env continue)
   (append-instruction-sequences
    (make-instruction-sequence
     '(env argl continue) '(env val continue)
     `(dolist-start
       (test (op no-more-exps?) (reg argl))
       (branch (label dolist-end))
       (assign continue (label dolist-start))
       (assign val (op first-exp) (reg argl))
       (assign env
               (op extend-environment)
               (const (,(dolist-var exp)))
               (reg val)
               (reg env))))
    (compile-sequence (dolist-body exp) 'val 'next 
                      (extend-compile-time-env
                       (list (dolist-var exp))
                       c-t-env)))
   (append-instruction-sequences
    (make-instruction-sequence '(env continue) '()
                               `(dolist-end))
    (compile% (dolist-term-exp exp) target linkage c-t-env))))
    
;(define (analyze-dolist exp)
;  (let ((varargs (cadr exp))
;        (list (analyze (cadr (cadr exp))))
;        (res (analyze (caddr (cadr exp))))
;        (exps (analyze-sequence (cdr (cdr exp)))))
;    (lambda (env)
;      (define (loop-dolist varlist)
;        (if (null? varlist)
;            (res env)
;            (begin
;              (exps (extend-environment (car varargs)
;                                        (car varlist)
;                                        env))
;              (loop-dolist (cdr varlist)))))
;      (loop-dolist (list env)))))

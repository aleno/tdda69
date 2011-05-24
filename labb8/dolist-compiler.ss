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

(define (dolist-body exp)
  (cddr exp))

(define (dolist-var exp)
  (caadr exp))
(define (dolist-args exp)
  (cadadr exp))
(define (dolist-term-exp exp)
  (car (cddadr exp)))

(define (compile-dolist exp target linkage c-t-env)
  (let ((start-label (make-label 'dolist-start))
        (end-label (make-label 'dolist-end)))
    (end-with-linkage
     linkage
     (preserving
      '(continue)
      (append-instruction-sequences
       (compile% (dolist-args exp) 'argl 'next c-t-env)
       (make-instruction-sequence
        '(argl) '(continue)
        `(,start-label
          (test (op no-more-exps?) (reg argl))
          (branch (label ,end-label))))
       (end-with-linkage
        start-label
        (preserving
         '(env)
         (append-instruction-sequences
          (make-instruction-sequence
           '(env argl continue) '(env argl val continue)
           `(
             (assign val (op first-exp) (reg argl))
             (assign val (op list) (reg val))
             (assign env
                     (op extend-environment)
                     (const (,(dolist-var exp)))
                     (reg val)
                     (reg env))))
          (preserving '(argl)
                      (compile-sequence (dolist-body exp) 'val 'next 
                                        (extend-compile-time-env
                                         (list (dolist-var exp))
                                         c-t-env))
                      (make-instruction-sequence '(argl) '(argl)
                                                 `((assign argl (op rest-exps) (reg argl))))))
         
         (make-instruction-sequence
          '(continue env argl) '(env)
          `()))))
      (append-instruction-sequences
       (make-instruction-sequence
        '() '()
        `(,end-label))
       (compile% (dolist-term-exp exp) target 'next c-t-env))))))
  
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

;; (compile-and-go '(%begin (%define x 1) (%dolist (%+ (%list %+ %* %-) x) (%set! x (%+ x 2)))))

; total pushes = 10 maximum depth = 4
; number of executed instructions = 95
; 4
; %==>

(load "compiler.ss")

(define (preprocess exp p-t-env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) exp)
        ((variable? exp) exp)
;        ((assignment? exp) (preprocess-assignment exp))
;        ((definition? exp) (preprocess-definition exp))
;        ((if? exp) (preprocess-if exp))
;        ((lambda? exp) (preprocess-lambda exp))
;        ((begin? exp) (preprocess-sequence (begin-actions exp)))
;        ((cond? exp) (preprocess (cond->if exp)))
        ((application? exp) (preprocess-application exp p-t-env))
        (else
         exp))) ;; error 'preprocess "Unknown expression type." exp))))

(define (primitive-application? exp)
  (or (tagged-list? exp '%+)
      (tagged-list? exp '%-)
      (tagged-list? exp '%*)
      (tagged-list? exp '%/)
      (tagged-list? exp '%=)
      (tagged-list? exp '%list)
      (tagged-list? exp '%cons)
      (tagged-list? exp '%car)
      (tagged-list? exp '%quote)))

(define (preprocess-application exp p-t-env)
  (let ((proc-code (preprocess (operator exp) p-t-env))
        (operand-codes
         (map (lambda (operand) (preprocess operand p-t-env))
              (operands exp))))
    (format "Proc: ~a~%" proc-code) (display operand-codes) (newline)
    (if (primitive-application? (cons proc-code operand-codes))
        (preprocess-primitive (cons proc-code operand-codes) p-t-env)
        exp)))

(define (get-primitive-implementation proc)
  (define (loop l)
    (if (null? l)
        'aaaaaa
        (if (eq? (caar l) proc)
            (cadr (car l))
            (loop (cdr l)))))
  (loop primitive-procedures))

(define (preprocess-primitive exp p-t-env)
  (display "prim") (display (operands exp)) (newline)
  (let ((proc (get-primitive-implementation (operator exp)))
        (quote-args (memq #t (map quoted? (operands exp))))
        (const-args (not (memq #f (map (lambda (x) 
                                         (or (self-evaluating? x)
                                             (quoted? x)
                                             (primitive-application? (cons x '()))))
                                       (operands exp))))))
    (display const-args)
    (display (format "I got ~a~%" exp))
    (cond ((not const-args) exp) ;; Ignorera om vi har något annat än constant.
          ((eq? proc cons)
           ;(if quote-args
           (display (operands exp)) (newline)
                    
               `(%quote ,(apply cons (map (lambda (x) (if (quoted? x) (cadr x) x))
                                          (operands exp))))
            ;   (apply cons (map (lambda (x) (if (quoted? x) (cadr x) x))
             ;                   (operands exp)))))
               )
          ((eq? proc list)
           `(%quote ,(apply list (map (lambda (x) (if (quoted? x) (cadr x) x))
                                      (operands exp)))))
          (else
           (display (format "Do ~a on ~a.~%" (operator exp) (operands exp)))
           (apply proc (map (lambda (x) (if (quoted? x) (cadr x) x))
                                          (operands exp)))))))

(define (compile-application exp target linkage c-t-env)
  (cond
    ((primitive-application? exp)
     (compile-primitive-application exp target linkage c-t-env))
    ((application? exp)
     (compile-normal-application exp target linkage c-t-env))
    (else
     (error "Unkown expression typ -- COMPILE" exp))))

(define (compile-primitive-application exp target linkage c-t-env)
  (let ((pp (preprocess exp c-t-env)))
    (display exp)
    (display pp)
    (display "CPA")
    (newline)
    pp))


(define (compile-normal-application exp target linkage c-t-env)
  (let ((proc-code (compile% (operator exp) 'proc 'next c-t-env))
        (operand-codes
         (map (lambda (operand) (compile% operand 'val 'next c-t-env))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage c-t-env)))))

(define-macro (test result exprected)
  `(if (equal? ,result ,exprected)
       'ok
       (error 'test "Test failed: ~a expected ~a.~%"
              ,result
              (quote ,exprected))))

(test
 (preprocess
  '(%- 3 4)
  '())
 -1)

(test
 (preprocess
  '(%cons (%+ 2 4) (%quote (hej hopp)))
  '())
 '(%quote (6 hej hopp)))

(test
 (preprocess
  '((%car (%list %+ %-)) 1 2)
  '())
 3)
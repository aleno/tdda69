(load "compiler.ss")

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile% (preprocess expression) 'val 'return initial-c-t-env))
                   eceval)))
    ;; (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag #t)
    (start eceval)))

(define (preprocess exp p-t-env)
  (cond ((self-evaluating? exp) exp)
        ((quoted? exp) exp)
        ((variable? exp) exp)
        ((assignment? exp) (preprocess-assignment exp p-t-env))
        ((definition? exp) (preprocess-definition exp p-t-env))
        ((if? exp) (preprocess-if exp p-t-env))
        ((lambda? exp) (preprocess-lambda exp p-t-env))
        ((begin? exp) (preprocess-sequence (begin-actions exp) p-t-env))
        ((cond? exp) (preprocess (cond->if exp) p-t-env))
        ((application? exp) (preprocess-application exp p-t-env))
        (else
         exp))) ;; error 'preprocess "Unknown expression type." exp))))

(define (preprocess-assignment exp p-t-env)
  `(,(operator exp) ,(car (operands exp)) ,(preprocess (cadr (operands exp)) p-t-env)))

(define (preprocess-definition exp p-t-env)
  (let ((p-t-env (cons (cdar (operands exp)) p-t-env)))
    `(%define ,(car (operands exp)) ,@(map (lambda (x) (preprocess x p-t-env)) (cdr (operands exp))))))
(define (preprocess-if exp p-t-env)
  ;; Optimera (%if #t exp1 exp2) -> exp1
  (let ((condition (preprocess (car (operands exp)) p-t-env))
        (exptected (preprocess (cadr (operands exp)) p-t-env))
        (alternativ (preprocess (caddr (operands exp)) p-t-env)))
    (cond ((eq? #t condition)
           exptected)
          ((eq? #f condition)
           alternativ)
          (else
           `(%if ,@(map (lambda (x) (preprocess x p-t-env)) (operands exp)))))))

(define (preprocess-lambda exp p-t-env)
  (let ((p-t-env (cons (lambda-parameters exp) p-t-env)))
    (display (format "p-t-env: ~a~%" p-t-env))
    `(%lambda ,(lambda-parameters exp) ,@(map (lambda (x) (preprocess x p-t-env)) (lambda-body exp)))))

(define (preprocess-sequence exp p-t-env)
  `(%begin ,@(map (lambda (x) (preprocess x p-t-env)) exp)))

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
    (if (and (not (assoc proc-code p-t-env))
             (primitive-application? (cons proc-code operand-codes)))
        (preprocess-primitive (cons proc-code operand-codes) p-t-env)
        exp)))

(define (get-primitive-implementation proc)
  (define (loop l)
    (if (null? l)
        '*no-primitive-impl-found*
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
           (display (operands exp)) (newline)
               `(%quote ,(apply cons (map (lambda (x) (if (quoted? x) (cadr x) x))
                                          (operands exp))))
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

(define (spread-arguments a b c-t-env)
  (list (compile% a 'arg1 'next c-t-env) (compile% b 'arg2 'next c-t-env)))

(define (compile-primitive-application exp target linkage c-t-env)
  (if (= (length (operands exp)) 2)
      (let ((args (spread-arguments (car (operands exp)) (cadr (operands exp)) c-t-env)))
        (end-with-linkage
         linkage
         (append-instruction-sequences
          (car args)
          (preserving '(arg1 proc)
                     (cadr args)
                     (make-instruction-sequence
                      '(arg1 arg2 proc)
                      (list target 'proc)
                      `((assign ,target
                                (op ,(operator exp))
                                (reg arg1)
                                (reg arg2))))))))
      (error 'compile-primitive-application "Emm. endast binära funktioner för tillfället.. =(")))

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

;; Makro som jämnför om en funktion resulterat ett resultat som matchar ett förväntat värde eller itne.
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

(test
 (preprocess
  '(%define (foo %+) (%+ 3 6))
  '())
 '(%define (foo %+) (%+ 3 6)))

;; Super optimerings test.
(test
 (preprocess
  '(%define (foo x y) (%if (%= (%+ 1 4) 5) (%cond ((%= (%- 3 1) 1) (%lambda (v w) (%+ 3 5))) ((%= (%- 3 1) 2) (%* 3 x)) (else (%cons 1 1))) (%begin (%+ 3 5) (%cons 4 x) (%- 4 2))))
  '())
 '(%define (foo x y) (%* 3 x)))
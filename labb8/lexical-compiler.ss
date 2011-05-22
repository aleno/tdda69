(load "compiler.ss")

;; Uppgift 5.41 SICP

(define (find-variable var env)
  ;; Sök igenom miljö ramar en för en.
  (define (env-loop frame-num env)
    ;; Sök igenom ramen efter variabeln.
    (define (scan disp-num vars)
      (cond ((null? vars)
             ;; Testa nästa ram.
             (env-loop (+ frame-num 1)
                       (enclosing-environment env)))
            ((eq? var (car vars))
             (make-lex-addr frame-num disp-num))
            (else (scan (+ 1 disp-num) (cdr vars)))))
    (if (eq? env '())
        'not-found
        (scan 0 (first-frame env))))
  (env-loop 0 env))

;; Uppgift 5.42 SICP

(define (compile-variable exp target linkage env)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    (let ((lex-addr (find-variable exp env)))
      (if (eq? lex-addr 'not-found)
          `((assign ,target
                    (op lookup-variable-value)
                    (const ,exp)
                    (reg env)))
          `((assign ,target
                    (op lexical-address-lookup)
                    (const ,lex-addr)
                    (reg env))))))))

(define (compile-assignment exp target linkage c-t-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile% (assignment-value exp) 'val 'next c-t-env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       (let ((lex-addr (find-variable var c-t-env)))
         (if (eq? lex-addr 'not-found)
             `((perform (op set-global-environment!)
                        (const ,var)
                        (reg val))
               (assign ,target (const ok)))
             `((perform (op lexical-address-set!)
                        (const ,lex-addr)
                        (reg val)
                        (reg env))
               (assign ,target (const ok))))))))))
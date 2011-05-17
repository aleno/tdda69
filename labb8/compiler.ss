
;;; --------------------------------------------------------------------------
;;;  compiler.ss
;;;  Compilation of %Scheme code into register machine code
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;;
;;; Uppdaterad för version 6 (mutable cons), Namnet compile numera en konstant.
;;; Proceduren compile -> compile% / AH 2009-11-25 --------------------------------------------------------------------------

;; (load "TDDA69/Lab/ec_eval.ss")
(load "/home/TDDA69/Lab/r6rs/ec_eval.ss")
;; (load "ec_eval.ss")

;;; --------------------------------------------------------------------------
;;;  Using the compiler
;;; --------------------------------------------------------------------------

(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements
                    (compile% expression 'val 'return initial-c-t-env))
                   eceval)))
    ;; (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag #t)
    (start eceval)))

;;; --------------------------------------------------------------------------
;;;  The compiler
;;; --------------------------------------------------------------------------
;; r6rs compile är en constant. compile ersatt med compile%
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
        ((application? exp)
         (compile-application exp target linkage c-t-env))
        (else
         (error 'compile "Unknown expression type: ~s" exp))))

;; Simple expressions

(define (compile-self-evaluating exp target linkage c-t-env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,exp))))))

(define (compile-quoted exp target linkage c-t-env)
  (end-with-linkage linkage
   (make-instruction-sequence '() (list target)
    `((assign ,target (const ,(text-of-quotation exp)))))))

(define (compile-variable exp target linkage c-t-env)
  (end-with-linkage linkage
   (make-instruction-sequence '(env) (list target)
    `((assign ,target
              (op lookup-variable-value)
              (const ,exp)
              (reg env))))))

(define (compile-assignment exp target linkage c-t-env)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile% (assignment-value exp) 'val 'next c-t-env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op set-variable-value!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

(define (compile-definition exp target linkage c-t-env)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile% (definition-value exp) 'val 'next c-t-env)))
    (end-with-linkage linkage
     (preserving '(env)
      get-value-code
      (make-instruction-sequence '(env val) (list target)
       `((perform (op define-variable!)
                  (const ,var)
                  (reg val)
                  (reg env))
         (assign ,target (const ok))))))))

;; Conditional expressions

(define (compile-if exp target linkage c-t-env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile% (if-predicate exp) 'val 'next c-t-env))
            (c-code
             (compile%
              (if-consequent exp) target consequent-linkage c-t-env))
            (a-code
             (compile% (if-alternative exp) target linkage c-t-env)))
        (preserving '(env continue)
         p-code
         (append-instruction-sequences
          (make-instruction-sequence '(val) '()
           `((test (op false?) (reg val))
             (branch (label ,f-branch))))
          (parallel-instruction-sequences
           (append-instruction-sequences t-branch c-code)
           (append-instruction-sequences f-branch a-code))
          after-if))))))

;; Sequences

(define (compile-sequence seq target linkage c-t-env)
  (if (last-exp? seq)
      (compile% (first-exp seq) target linkage c-t-env)
      (preserving '(env continue)
       (compile% (first-exp seq) target 'next c-t-env)
       (compile-sequence (rest-exps seq) target linkage c-t-env))))

;; Lambda expressions

(define (compile-lambda exp target linkage c-t-env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       (tack-on-instruction-sequence
        (end-with-linkage lambda-linkage
         (make-instruction-sequence '(env) (list target)
          `((assign ,target
                    (op make-compiled-procedure)
                    (label ,proc-entry)
                    (reg env)))))
        (compile-lambda-body exp proc-entry c-t-env))
       after-lambda))))

(define (compile-lambda-body exp proc-entry c-t-env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env
                (op extend-environment)
                (const ,formals)
                (reg argl)
                (reg env))))
     (compile-sequence (lambda-body exp) 'val 'return
		       (extend-compile-time-env formals c-t-env)))))

;; Combinations

(define (compile-application exp target linkage c-t-env)
  (let ((proc-code (compile% (operator exp) 'proc 'next c-t-env))
        (operand-codes
         (map (lambda (operand) (compile% operand 'val 'next c-t-env))
              (operands exp))))
    (preserving '(env continue)
     proc-code
     (preserving '(proc continue)
      (construct-arglist operand-codes)
      (compile-procedure-call target linkage c-t-env)))))

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
               code-to-get-last-arg
               (code-to-get-rest-args
                (cdr operand-codes))))))))

(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
          (car operand-codes)
          (make-instruction-sequence '(val argl) '(argl)
           '((assign argl
              (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
         code-for-next-arg
         (code-to-get-rest-args (cdr operand-codes))))))

;; Applying procedures

(define (compile-procedure-call target linkage c-t-env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage
           (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       (make-instruction-sequence '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       (parallel-instruction-sequences
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage c-t-env))
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
          (make-instruction-sequence '(proc argl)
                                     (list target)
           `((assign ,target
                     (op apply-primitive-procedure)
                     (reg proc)
                     (reg argl)))))))
       after-call))))

;; Applying compiled procedures

(define (compile-proc-appl target linkage c-t-env)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence '(proc) all-regs
           `((assign continue (label ,linkage))
             (assign val (op compiled-procedure-entry)
                         (reg proc))
             (goto (reg val)))))
        ((and (not (eq? target 'val))
              (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry)
                          (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry)
                        (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error 'compile-proc-appl "return linkage, target not val: ~s"
                target))))

;;; --------------------------------------------------------------------------
;;;  Compiler auxiliaries
;;; --------------------------------------------------------------------------

;; Linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next)
         (empty-instruction-sequence))
        (else
         (make-instruction-sequence '() '()
          `((goto (label ,linkage)))))))

(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
   instruction-sequence
   (compile-linkage linkage)))

;; Labels

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
    (string-append (symbol->string name)
                   (number->string (new-label-number)))))

;; Compile time environment

(define initial-c-t-env '())

(define (extend-compile-time-env params c-t-env)
  (cons params c-t-env))

;; The registers used by the compiled code

(define all-regs '(env proc val argl continue))

;;; --------------------------------------------------------------------------
;;;  Instruction sequences
;;; --------------------------------------------------------------------------

;; Creating instruction sequences

(define (make-instruction-sequence needs modifies statements)
  (list needs modifies statements))

(define (empty-instruction-sequence)
  (make-instruction-sequence '() '() '()))

;; Some selectors

(define (registers-needed s)
  (if (symbol? s) '() (car s)))

(define (registers-modified s)
  (if (symbol? s) '() (cadr s)))

(define (statements s)
  (if (symbol? s) (list s) (caddr s)))

(define (needs-register? seq reg)
  (memq reg (registers-needed seq)))

(define (modifies-register? seq reg)
  (memq reg (registers-modified seq)))

;; Combining instruction sequences

(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))

(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1)
                    (list-difference (cdr s1) s2)))))

(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
             (make-instruction-sequence
              (list-union (list first-reg)
                          (registers-needed seq1))
              (list-difference (registers-modified seq1)
                               (list first-reg))
              (append `((save ,first-reg))
                      (statements seq1)
                      `((restore ,first-reg))))
             seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))

(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))

;;; --------------------------------------------------------------------------

(display "Loaded compiler.ss")
(newline)

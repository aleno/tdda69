
;;; --------------------------------------------------------------------------
;;;  abssyntax.ss
;;;  Abstract syntax for the %Scheme language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).
;;; --------------------------------------------------------------------------

(define (atom? x)
  (or (number? x)
      (string? x)
      (symbol? x)
      (null? x)))
      
;; changed also to take care of mutable tagged-list (used in lazy.ss) 
;; / nov 2009 /AH
(define (tagged-list? exp tag)
  (cond ((pair? exp) (eq? (car exp) tag))
        ((mpair? exp) (eq? (mcar exp) tag))                 
        (else #f)))

(define (true? x)
  (not (eq? x #f)))

(define (false? x)
  (eq? x #f))

;; "Atomic" expressions
;; --------------------

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (quoted? exp)
  (tagged-list? exp '%quote))

(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))

;; Assignment (%set!)
;; ------------------

(define (assignment? exp) (tagged-list? exp '%set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

;; Definition (%define)
;; --------------------

(define (definition? exp)
  (tagged-list? exp '%define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

;; (%define (fn args) body) is expanded to (%define fn (lambda (args) body))

;; Lambda (%lambda)
;; ----------------

(define (lambda? exp) (tagged-list? exp '%lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons '%lambda (cons parameters body)))

;; Sequences (%begin)
;; ------------------

;; In this section a sequence (denoted by the argument name seq) is
;; simply a list of expressions extracted from a %begin expression
;; in %scheme.

(define (begin? exp) (tagged-list? exp '%begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons '%begin seq))

;; Applications
;; ------------

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

;; Selection (%if)
;; ---------------

(define (if? exp) (tagged-list? exp '%if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      '%false))

(define (make-if predicate consequent alternative)
  (list '%if predicate consequent alternative))

;; Selection (%cond)
;; -----------------

;; A %cond is rewritten to a %if expression.

(define (cond? exp) (tagged-list? exp '%cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) '%else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      '%false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error 'expand-clauses "%else clause isn't last: ~s"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

;;; --------------------------------------------------------------------------

(display "Loaded abssyntax.ss")
(newline)


;;; --------------------------------------------------------------------------
;;;  scmlog.ss
;;;  Evaluator for the logic programming language
;;;
;;;  Original code from "Structure and Interpretation of Computer Programs"
;;;  by Abelson & Sussman. Adapted for use in the course "Data and Program
;;;  Structures" (TDDA69).

;;; Ändrad till Scheme version 6 (mutable cons). Namnet instantiate är 
;;; numera en konstant. Proceduren instantiate -> instantiate-qlog 
;;; / AH 2009-11-25
;;; --------------------------------------------------------------------------

;;; ------------------------------------------------------------------------
;;;  1. Datalagring
;;; ------------------------------------------------------------------------

;; Nedanst�ende tabellstruktur anv�nds f�r att lagra dels de operationer i 
;; evaluatorn som anv�nds f�r sammansatta f�rfr�gningar (t.ex. and), dels
;; alla fakta och regler som anv�ndaren deklarerar.

;; Strukturen best�r egentligen av flera tabellen och representeras som 
;; associationslistor i tv� niv�er, vilket inneb�r att data h�mtas och 
;; skickas in med tv� nycklar. Sj�lva tabellen �r ett procedurobjekt
;; (dispatch-funktionen).

(define (massoc key alist)
  (cond ((null? alist) '())
        ((eq? key (mcar (mcar alist))) (mcar alist))
        (else (massoc key (mcdr alist)))))

(define (make-table)
  (let ((local-table (mcons '*table* '())))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
	(if (mpair? subtable)
	    (let ((record (massoc key-2 (mcdr subtable))))
	      (if (mpair? record)
		  (mcdr record)
		  #f))
	    #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
	(if (mpair? subtable)
	    (let ((record (massoc key-2 (mcdr subtable))))
	      (if (mpair? record)
		  (set-mcdr! record value)
		  (set-mcdr! subtable
			    (mcons (mcons key-2 value)
                                   (mcdr subtable)))))
            (set-mcdr! local-table
                       (mcons (mcons key-1
                                     (mcons (mcons key-2 value) '()))
                              (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
	    ((eq? m 'insert!) insert!)
	    ((eq? m 'dump) (lambda () local-table))
	    (else (error 'table "Unknown operation ~s." m))))
    dispatch))

;; Den globala tabellstruktur som anv�nds i evaluatorn kallas operation-table,
;; men �tkomst sker uteslutande med hj�lpfunktionerna put och get som l�gger 
;; in respektive h�mtar information fr�n tabellerna.

(define operation-table (make-table))

(define put (operation-table 'insert!))

(define get (operation-table 'lookup))

;; I praktiken kan man se det som att evaluatorn anv�nde tre tabeller
;; som alla lagras i samma struktur: 
;;
;;   qeval              inneh�ller evaluatorns operationer
;;   assertion-stream   inneh�ller alla fakta
;;   rule-stream        inneh�ller alla regler
;;
;; Operationerna put och get anv�nds enligt nedan:
;; 
;;   (put 'NYCKELORD 'TABELL DATA)
;;   (get 'NYCKELORD 'TABELL) --> DATA
;;
;; Po�ngen med att anv�nda det h�r s�ttet �r dels att det blir mycket
;; l�tt att ut�ka evaluatorn med fler operationer, dels att det blir
;; l�ttare att hitta fakta och regler, eftersom dessa p� detta s�tt indexeras
;; p� f�rsta symbolen i listan.

;;; ------------------------------------------------------------------------
;;;  2. Str�mfunktioner
;;; ------------------------------------------------------------------------

;; F�rst laddar vi in de existerande str�mfunktionerna som vi anv�nt
;; i tidigare laborationer:

;;  (load "/home/TDDA69/Lab/streams.ss"))
;;(load "/home/TDDA69/Lab/r6rs/streams.ss")
(load "streams.ss")

;; Funktionen flatmap applicerar en funktion p� varje element i en str�m.
;; Resultatet av varje s�dan applikation �r en ny str�m och dessa nya
;; str�mmar s�tts ihop med hj�lp av interleave.

(define (flatmap proc stream1)
  (if (stream-null? stream1)
      the-empty-stream
      (let ((stream2 (proc (stream-car stream1))))
        (if (stream-null? stream2)
            (flatmap proc (stream-cdr stream1))
            (cons-stream (stream-car stream2)
                         (interleave (flatmap proc (stream-cdr stream1))
                                     (stream-cdr stream2)))))))

;; Singleton skapar en str�m med endast ett element.

(define (singleton s)
  (cons-stream s the-empty-stream))

;; Interleave s�tter ihop tv� str�mmar genom att ta element �msom fr�n
;; den ena och �msom fr�n den andra str�mmen.

(define (interleave stream1 stream2)
  (if (stream-null? stream1)
      stream2
      (cons-stream (stream-car stream1)
                   (interleave stream2
                               (stream-cdr stream1)))))

;; Funktionen interleave-delayed g�r i princip samma sak som interleave, 
;; men g�r delay p� resten (och kr�ver att den andra str�mmen ska vara
;; delayad).

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (interleave-delayed (force delayed-s2)
                                       (delay (stream-cdr s1))))))

;; Funktionen append-stream sl�r ihop tv� str�mmar genom att l�gga elementen
;; i den andra str�mmen sist i den f�rsta.

(define (append-streams stream1 stream2)
  (if (stream-null? stream1)
      stream2
      (cons-stream (stream-car stream1)
                   (append-streams (stream-cdr stream1) stream2))))

(define (append-delayed s1 delayed-s2)
  (if (stream-null? s1)
      (force delayed-s2)
      (cons-stream (stream-car s1)
                   (append-delayed (stream-cdr s1) delayed-s2))))

(define (print-stream stream)
  (if (stream-null? stream)
      (newline)
      (begin 
	(newline)
	(display (stream-car stream))
	(print-stream (stream-cdr stream)))))

(define (print-stream-elements-on-separate-lines s)
  (if (stream-null? s)
      (display 'done)
      (begin (display (stream-car s))
	     (newline)
	     (print-stream-elements-on-separate-lines
	      (stream-cdr s)))))

;;; ------------------------------------------------------------------------
;;;  3. Huvudfunktioner
;;; ------------------------------------------------------------------------

;; Detta �r huvudfunktionen som l�ser in och agerar p� kommandon och
;; uttryck. Det finns i princip tre fall:
;;
;;  - det atom�ra kommandot 'exit' f�r att avsluta
;;  - ett sammansatt kommando ('qload' eller 'print-db') som
;;    mappas direkt till motsvarande funktion i Scheme via execute-command
;;  - fr�gor som f�rst skickas genom query-syntax-process som expanderar
;;    alla variabler '?x' till '(? x)' f�r att l�ttare kunna behandla dem
;;    i hj�lpfunktionen query-driver-loop-2

(define (query-driver-loop)	       
  (newline)
  (display "query==> ")
  (let ((expr (read)))
    (cond ((not (pair? expr))
	   (if (eq? expr 'exit)
	       'done
	       (begin
		 (display "Illegal query!")
		 (query-driver-loop))))
	  ((command? expr)
	   (execute-command expr)
	   (query-driver-loop))
	  (else
	    (query-driver-loop-2 (query-syntax-process expr))))))

(define (query-driver-loop-2 q)
  (cond ((assertion-to-be-added? q)
	 (add-rule-or-assertion! (add-assertion-body q))
	 (display "Assertion added to data base."))
	((group-to-be-added? q)
	 (if (equal-rule-names? (rules q))
	     (begin 
	       (remove-group! (rule-or-assertion-name (first-rule (rules q))))
	       (add-group! (rules q))
	       (display "Group added to data base."))
	     (display "Different names in group is not allowed!")))
	((group-to-be-removed? q)
	 (remove-group! (rule-or-assertion-name (rules q)))
	 (display "Group removed from data base."))
	(else
	 (print-stream-elements-on-separate-lines
	  (stream-map (lambda (frame)
			(instantiate-qlog q
				     frame
				     (lambda (v f)
				       (contract-question-mark v))))
		      (qeval q (singleton '()))))))
  (query-driver-loop))

(define (instantiate-qlog exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((constant? exp) exp)
          ((var? exp)
           (let ((vcell (binding-in-frame exp frame)))
             (if (not vcell)
                 (unbound-var-handler exp frame)
                 (copy (binding-value vcell)))))
          (else (cons (copy (car exp))
                      (copy (cdr exp))))))
  (copy exp))

;; Funktionen qeval �r den centrala funktionen f�r att svara p� fr�gor.
;; Fr�gans typ, dvs f�rsta elementet i listan, sl�s upp i operationstabellen.
;; Om en procedur hittas inneb�r det att det var en giltig sammansatt
;; fr�ga och den aktuella proceduren (n�gon i nedanst�ende avsnitt) anropas.
;; I annat fall kontrolleras om p�st�endet kan verifieras med hj�lp av
;; aktuella fakta och regler med hj�lp av funktionen asserted?

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
        (qproc (contents query) frame-stream)
        (asserted? query frame-stream))))

;;; ------------------------------------------------------------------------
;;;  4. Hantering av enkla fr�gor
;;; ------------------------------------------------------------------------

(define (asserted? query-pattern frame-stream)
  (flatmap (lambda (frame)
             (append-delayed
	      (find-assertions query-pattern frame)
	      (delay (apply-rules query-pattern frame))))
           frame-stream))

(define (find-assertions pattern frame)
  (flatmap (lambda (datum)
             (pattern-match pattern datum frame))
           (fetch-assertions pattern frame)))

(define (pattern-match pat dat frame)
  (let ((result (internal-match pat dat frame)))
    (if (eq? result 'failed)
        the-empty-stream
        (singleton result))))

(define (internal-match pat dat frame)
  (cond ((eq? frame 'failed) 'failed)
        ((var? pat) (extend-if-consistent pat dat frame))
        ((constant? pat)
         (if (constant? dat)
             (if (same-constant? pat dat) frame 'failed)
             'failed))
        ((constant? dat) 'failed)
        (else (internal-match (cdr pat)
                              (cdr dat)
                              (internal-match (car pat)
                                              (car dat)
                                              frame)))))

(define (extend-if-consistent var dat frame)
  (let ((value (binding-in-frame var frame)))
    (if (not value)
        (extend var dat frame)
        (internal-match (binding-value value) dat frame))))


(define (apply-rules pattern frame)
  (flatmap (lambda (rule)
             (apply-a-rule rule pattern frame))
           (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (stream-null? unify-result)
          the-empty-stream
          (qeval (rule-body clean-rule) unify-result)))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((constant? exp) exp)
            ((var? exp)
             (make-new-variable exp rule-application-id))
            (else (cons (tree-walk (car exp))
                        (tree-walk (cdr exp))))))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (let ((result (internal-unify p1 p2 frame)))
    (if (eq? result 'failed)
        the-empty-stream
        (singleton result))))

(define (internal-unify p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((constant? p1)
         (if (constant? p2)
             (if (same-constant? p1 p2) frame 'failed)
             'failed))
        ((constant? p2) 'failed)
        (else (internal-unify (cdr p1)
                              (cdr p2)
                              (internal-unify (car p1)
                                              (car p2)
                                              frame)))))

(define (extend-if-possible var val frame)
  (if (equal? var val)
      frame
      (let ((value-cell (binding-in-frame var frame)))
        (if (not value-cell)
            (if (freefor? var val frame)
                (extend var val frame)
                'failed)
            (internal-unify (binding-value value-cell)
                            val
                            frame)))))

(define (freefor? var exp frame)
  (define (freewalk e)
    (cond ((constant? e) #t)
          ((var? e)
           (if (equal? var e) #f
               (let ((b (binding-in-frame e frame)))
                 (if (not b) #t
                     (freewalk (binding-value b))))))
          ((freewalk (car e)) (freewalk (cdr e)))
          (else #f)))
  (freewalk exp))

;;; ------------------------------------------------------------------------
;;;  5. Hantering av sammansatta fr�gor
;;; ------------------------------------------------------------------------

;; Funktion i       Funktion i
;; logikspr�ket     evaluatorn
;; --------------------------------
;;   and              conjoin
;;   or               disjoin
;;   not              negate
;;   lisp-value       lisp-value
;;   always-true      always-true

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
      frame-stream
      (conjoin (rest-conjuncts conjuncts)
               (qeval (first-conjunct conjuncts)
                      frame-stream))))

(put 'and 'qeval conjoin)

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
      the-empty-stream
      (interleave-delayed
       (qeval (first-disjunct disjuncts) frame-stream)
       (delay (disjoin (rest-disjuncts disjuncts)
		       frame-stream)))))

(put 'or 'qeval disjoin)

(define (negate a frame-stream)
  (flatmap
   (lambda (frame)
     (if (stream-null? (qeval (negated-query a)
			      (singleton frame)))
	 (singleton frame)
	 the-empty-stream))
   frame-stream))

(put 'not 'qeval negate)

;; Utökning uppgift 7: unique

(define (uniquely-asserted a frame-stream)
  (flatmap
   (lambda (frame)
     (let ((contents (qeval (car a) (singleton frame))))
       (cond ((stream-null? contents)
              the-empty-stream)
             ((stream-null? (stream-cdr contents))
              contents)
             (else
              the-empty-stream))))
   frame-stream))

;; Den triviala delen. :)
(put 'unique 'qeval uniquely-asserted)

(define (lisp-value call frame-stream)
  (flatmap
   (lambda (frame)
     (if (execute
	  (instantiate-qlog 
	   call
	   frame
	   (lambda (v f)
	     (error 'lisp-value "~s is an unknown pattern variable." v))))
	 (singleton frame)
	 the-empty-stream))
   frame-stream))

(put 'lisp-value 'qeval lisp-value)

;; Nytt i r6rs, behövs detta? Skalldet vara något annat initial name-space?/ AH
(define user-initial-environment (current-namespace))

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream)
  frame-stream)

(put 'always-true 'qeval always-true)

;;; ------------------------------------------------------------------------
;;;  6. Kunskapsbasen
;;; ------------------------------------------------------------------------

;; Detta avsnitt hanterar kunskapsbasen, dvs fakta (eng. assertions)
;; och regler (eng. rules).

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
      (get-indexed-assertions pattern)
      (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if (not s) the-empty-stream s)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
      (get-indexed-rules pattern)
      (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (append-streams
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
      (add-rule! assertion)
      (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
          (cons-stream assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (remove-group! group-name)
  (remove-group-from-index group-name)
  (let ((old-assertions THE-ASSERTIONS)
	(old-rules THE-RULES))
    (set! THE-ASSERTIONS (remove-all-assertions group-name old-assertions))
    (set! THE-RULES (remove-all-rules group-name old-rules))
    'ok))

(define (remove-all-assertions name assertions)
  (stream-filter
   (lambda (assertion) (not (eq? name (rule-or-assertion-name assertion))))
   assertions))

(define (remove-all-rules name rules)
  (stream-filter
   (lambda (rule) (not (eq? name (rule-or-assertion-name rule))))
   rules))

(define (add-group! rules)
  (for-each add-rule-or-assertion! (reverse rules))
  'ok)

(define (equal-rule-names? rules)
  (let ((ok-flg #t)
	(name (rule-or-assertion-name (first-rule rules))))
    (for-each (lambda (rule-or-assertion)
		(if (not (eq? name 
			      (rule-or-assertion-name rule-or-assertion)))
		    (set! ok-flg #f)))
	      rules)
    ok-flg))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
      (let ((key (index-key-of assertion)))
        (let ((current-assertion-stream
	       (get-stream key 'assertion-stream)))
          (put key
               'assertion-stream
               (cons-stream assertion
                            current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
        (let ((key (index-key-of pattern)))
          (let ((current-rule-stream
		 (get-stream key 'rule-stream)))
            (put key
                 'rule-stream
                 (cons-stream rule
                              current-rule-stream)))))))

(define (remove-group-from-index key)
  (put key 'assertion-stream #f)
  (put key 'rule-stream #f))

;;; ------------------------------------------------------------------------
;;;  7. Primitiva funktioner
;;; ------------------------------------------------------------------------

;; Diverse testfunktioner f�r uttryck
;; ----------------------------------

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

(define (type exp)
  (if (not (pair? exp))
      (error 'type "~s is an unknown expression." exp)
      (if (symbol? (car exp)) (car exp) #f)))

(define (contents exp)
  (if (not (pair? exp))
      (error 'contents "~s is an unknown expression." exp)
      (cdr exp)))

;; Testfunktioner f�r vissa kommandon
;; ----------------------------------

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

(define (group-to-be-added? exp)
  (eq? (type exp) 'assert-group!))

(define (group-to-be-removed? exp)
  (eq? (type exp) 'retract-group!))

(define (add-assertion-body exp)
  (car (contents exp)))

;; Abstrakt syntax f�r sammansatta fr�gor
;; --------------------------------------

(define empty-conjunction? null?)

(define first-conjunct car)

(define rest-conjuncts cdr)

(define empty-disjunction? null?)

(define first-disjunct car)

(define rest-disjuncts cdr)

(define negated-query car)

(define predicate car)

(define args cdr)

;; Abstrakt syntax f�r regler
;; --------------------------

(define (rule? statement)
  (if (not (pair? statement))
      #f
      (eq? (car statement) 'rule)))

(define conclusion cadr)

(define (rule-body rule)
  (if (null? (cddr rule))
      '(always-true)
      (caddr rule)))

(define (rules s)
  (if (pair? s) (cdr s)))

(define (first-rule s)
  (if (pair? s) (car s)))

(define (rule-or-assertion-name r)
  (if (and (pair? r) (rule? r))
      (caadr r)
      (car r)))

;; Omvandling mellan extern och intern notation f�r variabler
;; ----------------------------------------------------------

;; Funktionen query-syntax-process g�r igenom ett uttryck och ers�tter
;; alla variabler ?x med (? x) f�r att man l�ttare ska kunna bearbeta
;; uttrycket.

(define (query-syntax-process exp)
  (map-over-atoms expand-question-mark exp))

(define (map-over-atoms proc exp)
  (if (not (pair? exp))
      (proc exp)
      (cons (map-over-atoms proc (car exp))
            (map-over-atoms proc (cdr exp)))))

(define (expand-question-mark atom)	
  (if (symbol? atom)
      (let ((str (symbol->string atom)))
	(if (eq? (string-ref (symbol->string atom) 0) #\?)
	    (list '? (string->symbol (substring str 1 (string-length str))))
	    atom))
      atom))

;; Funktionen contract-question-mark ers�tter en variabel representerad
;; som en lista (? x) med symbolen ?x.

(define (contract-question-mark variable)
  (string->symbol           
   (string-append "?" (symbol->string
		       (if (number? (cadr variable))
			   (caddr variable)
			   (cadr variable))))))

(define (var? exp)
  (if (not (pair? exp))
      #f
      (eq? (car exp) '?)))

(define (restore-variables list)
  (cond ((var? list) (contract-question-mark list))
	((not (pair? list)) list)
	(else (cons (restore-variables (car list))
		    (restore-variables (cdr list))))))

;; ----------

(define (constant? exp) (not (pair? exp)))

(define constant-symbol? symbol?)

(define same-constant? equal?)

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ rule-counter 1))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))

;; Bindningar
;; ----------

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

;;; ------------------------------------------------------------------------
;;;  8. Kommandon
;;; ------------------------------------------------------------------------

;; K�nner igen kommandon till interpretatorn

(define (command? c)
  (and (pair? c)
       (or
         (eq? (car c) 'qload)	 
         (eq? (car c) 'print-db))))

;; K�r ett kommando, dvs mappar det direkt till motsvarande Scheme-
;; funktion med hj�lp av eval
	 
(define execute-command eval)

;; Kommandot qload laddar in en fil med fakta och regler

(define (qload file)
  (let ((aqum '())
	(removed '())
	(port (open-input-file file)))
    (do ((x (read port) (read port)))
        ((eof-object? x) (close-input-port port))
	(let ((name (rule-or-assertion-name x)))
	  (if (not (member name removed))
	      (begin 
		(remove-group! name)
		(set! removed (cons name removed))
		(display name)
		(newline)))
	  (set! aqum (cons (query-syntax-process x) aqum))))
    (for-each add-rule-or-assertion! aqum)))

;; Kommandot print-db skriver ut aktuella fakta och regler

(define (print-db)
  (print-stream THE-ASSERTIONS)
  (print-stream (stream-map restore-variables THE-RULES))
  'done)



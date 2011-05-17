
(define fac-machine
  (make-machine
   '(n val continue)
   `((= ,=)
     (- ,-)
     (* ,*)
     (display ,display)
     (newline ,newline))
   '(controller
    (assign  continue (label fact-done))
    fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
    after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val))
    (goto (reg continue))
    base-case
    (assign  val (const 1))
    (goto (reg continue))
    fact-done
    (perform (op print-statistics))
    (perform (op display) (const "Resultat: "))
    (perform (op display) (reg val))
    (perform (op newline)))))
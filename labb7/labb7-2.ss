;; Uppgift 2

Ändra ec_eval.ss med följande:

ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))
...
ev-sequence-end
  (restore continue)
  (goto (reg continue))

  
			| Maxdjup     | Antal push
------------+-------------+-------------
Iterativ    | 14 + 3 * n  | 33 + 37 * n
------------+-------------+-------------
Rekursiv    | 3 + 8 * n   | 34 * n - 16
------------+-------------+-------------

;; Kontroll..

%==> (factorial-rec 100)

total pushes = 3384 maximum depth = 803
number of executed instructions = 34666
...

34 * 100 - 16 = 3384  OK!
3 + 8 * 100   = 803   OK!

%==> (factorial-iter 100)

total pushes = 3733 maximum depth = 314
number of executed instructions = 37368
...

33 + 37 * 100 = 3733  OK!
14 + 3 * 100  = 314   OK!

%==> (%define (factorial-iter n)
(%define (iter product counter)
(%cond ((%> counter n) product)
(%else (iter (%* counter product)
(%+ counter 1)))))
(iter 1 1))

total pushes = 3 maximum depth = 3
number of executed instructions = 55
ok
%==> (factorial-iter 1)

total pushes = 70 maximum depth = 17
number of executed instructions = 738
1
%==> (factorial-iter 2)

total pushes = 107 maximum depth = 20
number of executed instructions = 1108
2
%==> (factorial-iter 3)

total pushes = 144 maximum depth = 23
number of executed instructions = 1478
6

%==> (%define (factorial-rec n)
(%cond ((%= n 1) 1)
(%else (%* (factorial-rec (%- n 1)) n))))

total pushes = 3 maximum depth = 3
number of executed instructions = 55
ok
%==> (factorial-rec 1)

total pushes = 18 maximum depth = 11
number of executed instructions = 214
1
%==> (factorial-rec 2)

total pushes = 52 maximum depth = 19
number of executed instructions = 562
2
%==> (factorial-rec 3)

total pushes = 86 maximum depth = 27
number of executed instructions = 910
6

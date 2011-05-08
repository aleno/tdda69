;; Uppgift 1

            | Maxdjup     | Antal push
------------+-------------+-------------
Iterativ    | 10          | 29 + 35 * n
------------+-------------+-------------
Rekursiv    | 3 + 5 * n   | 32 * n - 16
------------+-------------+-------------

;; Kontroll..

%==> (factorial-rec 100)

total pushes = 3184 maximum depth = 503
number of executed instructions = 33666
...

32 * 100 - 16 = 3184  OK..
3 + 5 * 100 = 503  OK..

%==> (factorial-iter 100)

total pushes = 3529 maximum depth = 10
number of executed instructions = 36348
...

29 + 35 * 100 = 3529  OK..
Stackdjup oförändrat (10) OK..


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

total pushes = 64 maximum depth = 10
number of executed instructions = 708
1
%==> (factorial-iter 2)

total pushes = 99 maximum depth = 10
number of executed instructions = 1068
2

%==> (factorial-iter 3)

total pushes = 134 maximum depth = 10
number of executed instructions = 1471
6

%==> (%define (factorial-rec n)
(%cond ((%= n 1) 1)
(%else (%* (factorial-rec (%- n 1)) n))))

total pushes = 3 maximum depth = 3
number of executed instructions = 55
ok
%==> (factorial-rec 1)

total pushes = 16 maximum depth = 8
number of executed instructions = 204
1
%==> (factorial-rec 2)

total pushes = 48 maximum depth = 13
number of executed instructions = 542
2
%==> (factorial-rec 3)

total pushes = 80 maximum depth = 18
number of executed instructions = 880
6

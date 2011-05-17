;; Uppgift 1

            | Maxdjup     | Antal push
------------+-------------+-------------
Iterativ    | 3           | 7 + 6 * n
------------+-------------+-------------
Rekursiv    | 3 * n - 1 � | 1 + 6 * n
------------+-------------+-------------
Fac-machine | 2 * n - 2   | 2 * n - 2   
------------+-------------+-------------

(�) Maxdjupet �r 3 f�r n = 1.

Fakultets maskin �r effektivare �n general purpose maskinen.
Detta beror p� att GP maskinen m�ste ber�kna argument och g�ra
variabel uppslagningar n�r den k�rs vilket ger upphov till 
st�rre djup och mer pushar. 

Fakultets maskinen sparar endast f�reg�ende v�rde N och 
�terhoppsadressen p� stacken. Den sparar �ven sina v�rden direkt
i registren ist�llet f�r att sl� upp variabelv�det.

;; Kontroll..

%==> (factorial-iter 300)

total pushes = 1807 maximum depth = 3
number of executed instructions = 14208
...

7 + 6 * 300 = 1807  OK..
Stackdjup of�r�ndrat (3)  OK..

%==> (factorial-rec 100)

total pushes = 601 maximum depth = 299
number of executed instructions = 4746
...

1 + 6 * 100 = 601  OK..
3 * 100 - 1 = 299  OK..

> (load "fac-machine.ss")
> (set-register-contents! fac-machine 'n 100)
done
> (start fac-machine)

total pushes = 198 maximum depth = 198
number of executed instructions = 1097

2 * 100 - 2 = 198  OK..
2 * 100 - 2 = 198  OK..

;; Utdrag fr�n prompt.
%==> (compile-and-go
'(%define (factorial-rec n)
(%cond ((%= n 1) 1)
(%else (%* (factorial-rec (%- n 1)) n)))))

total pushes = 0 maximum depth = 0
number of executed instructions = 20
ok
%==> (factorial-rec 1)

total pushes = 7 maximum depth = 3
number of executed instructions = 93
1
%==> (factorial-rec 2)

total pushes = 13 maximum depth = 5
number of executed instructions = 140
2
%==> (factorial-rec 3)

total pushes = 19 maximum depth = 8
number of executed instructions = 187
6
%==> (factorial-rec 4)

total pushes = 25 maximum depth = 11
number of executed instructions = 234
24
%==> (factorial-rec 5)

total pushes = 31 maximum depth = 14
number of executed instructions = 281
120
(compile-and-go
 '(%define (factorial-iter n)
    (%define (iter product counter)
             (%cond ((%> counter n) product)
                    (%else (iter (%* counter product)
                                 (%+ counter 1)))))
    (iter 1 1)))

total pushes = 0 maximum depth = 0
number of executed instructions = 54
ok
%==> (factorial-iter 1)

total pushes = 13 maximum depth = 3
number of executed instructions = 155
1
%==> (factorial-iter 2)

total pushes = 19 maximum depth = 3
number of executed instructions = 202
2
%==> (factorial-iter 3)

total pushes = 25 maximum depth = 3
number of executed instructions = 249
6

;; Uppgift 1

            | Maxdjup     | Antal push
------------+-------------+-------------
Iterativ    | 3           | 7 + 6 * n
------------+-------------+-------------
Rekursiv    | 3 * n - 1 ¤ | 1 + 6 * n
------------+-------------+-------------
Fac-machine | 2 * n - 2   | 2 * n - 2   
------------+-------------+-------------

(¤) Maxdjupet är 3 för n = 1.

Fakultets maskin är effektivare än general purpose maskinen.
Detta beror på att GP maskinen måste beräkna argument och göra
variabel uppslagningar när den körs vilket ger upphov till 
större djup och mer pushar. 

Fakultets maskinen sparar endast föregående värde N och 
återhoppsadressen på stacken. Den sparar även sina värden direkt
i registren istället för att slå upp variabelvädet.

;; Kontroll..

%==> (factorial-iter 300)

total pushes = 1807 maximum depth = 3
number of executed instructions = 14208
...

7 + 6 * 300 = 1807  OK..
Stackdjup oförändrat (3)  OK..

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

;; Utdrag från prompt.
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

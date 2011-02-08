(load "/home/TDDA69/Lab/r6rs/streams.ss")

(define S
  (cons-stream 1
               (merge (merge (scale-stream 2 S) (scale-stream 3 S)) (scale-stream 5 S))))

(define S405
  (stream-filter (lambda (x) (> x 405)) S))

(print-stream S 20)
; [ 1 2 3 4 5 6 8 9 10 12 15 16 18 20 24 25 27 30 32 36 ... ]

(print-stream S405 6)
; [ 432 450 480 486 500 512 ... ]

; Delupggift b
; Man måste dela ett tal X med 2, 3, 5 tills de inte går att dela mer med vardera tal och sedan kontrollera om talet som återstår är 1. Om det inte är ett så innehåller talet andra primtals faktorer än 2,3,5 och ska då filtreras bort. Att testa dela ett tal flera ggr är en jobbig operation. Därför är lösningen i a mycket bättre för vi behöver inte utföra någon division och kontroll av talet.

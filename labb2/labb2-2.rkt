(load "/home/TDDA69/Lab/r6rs/streams.ss")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define not-three
  (stream-filter (lambda (x) (not (divisible? x 3))) integers))

(define not-seven
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))


(load "/home/TDDA69/Lab/r6rs/streams.ss")

(define ones (cons-stream 1 ones))
(define integers (cons-stream 1 (add-streams ones integers)))

(define not-two-three-five 
  (stream-filter 
   (lambda (x) 
     (cond ((= (modulo x 2) 0) #f)
           ((= (modulo x 3) 0) #f)
           ((= (modulo x 5) 0) #f)
           (else #t))) integers))

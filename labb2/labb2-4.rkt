(load "/home/TDDA69/Lab/r6rs/streams.ss")
(require rnrs/mutable-pairs-6)
(define a
  (cons-stream 1
               (cons-stream 2
                            (cons-stream 3 the-empty-stream))))

(define (my-cdr stream)
  (set-cdr! stream
            (cons (force (cdr stream))
                  '(delay ,b))))
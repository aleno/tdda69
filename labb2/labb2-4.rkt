(load "/home/TDDA69/Lab/r6rs/streams.ss")

; (require rnrs/mutable-pairs-6)

(define-macro mcons-stream
  (lambda (a b)
    `(mcons ,a (delay ,b))))

(define a
  (mcons-stream 1
               (mcons-stream 2
                            (mcons-stream 3 the-empty-stream))))

(define (my-cdr stream)
  (set-mcdr! stream
            (force (mcdr stream)))
  (mcdr stream))
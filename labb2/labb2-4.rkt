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

; våran cdr funktion ersätter den funktion som conscellen pekar på med en ny conscell som har värdet av vad beräkningen skulle ge samt beräkningen för cellen därefter.
; Detta gör att om du skulle utföra 3st cdr på våran ström och sedan utföra 2st cdr på samma ström som innan, så skulle inte värdena behövas beräknas andra gången eftersom dessa redan är beräknade.
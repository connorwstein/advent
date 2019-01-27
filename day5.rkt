#lang racket

; Make continuous passes over the string 
; Worst case is something like dcbaABCD which is 
; O(N^2) because we only chop one pair per each passes i.e. N + N-2 + N-4 ... N-(N+2) + N-N = (N^2)/2 + (1 + 2 ... (N-1)) = O(N^2)
(define input (list-ref (file->lines "input_day5.txt") 0))
(define (should-react c1 c2)
    ; return t/f depending on whether c1 and c2 should react
    (eq? (abs (- (char->integer c1) (char->integer c2))) 32)
)
(define (react start)
    (define continue #t)
    ; pass takes a vector and returns a vector of the result
    (define (pass input)
        ; One pass reaction
        (define input-length (vector-length input))
        (set! continue #f)
        (define (helper i j res)
            (cond ((eq? j (- input-length 1)) 
			(let ([c1 (vector-ref input i)]
			      [c2 (vector-ref input j)])
					(list->vector (reverse (if (should-react c1 c2) 
												res 
											   (cons c2 (cons c1 res))))))) ; compare the last 2 and handle appropriately --> same were done, different copy both
				  ((eq? i (- input-length 1)) 
					(list->vector (reverse (cons (vector-ref input i) res)))); just copy last guy
                (else (if (should-react (vector-ref input i) (vector-ref input j))
                    (begin (set! continue #t) (helper (+ 2 i) (+ 2 j) res))
                    (helper (+ 1 i) (+ 1 j) (cons (vector-ref input i) res))))))
        (helper 0 1 '())
    )
    ; Keep calling an input, getting an output
    ; then feeding it back in UNLESS continue is false 
    (define (run in)
        (if (not continue) 
            (list->string (vector->list in))
            (run (pass in)))
    )
    (run start)
)
; Stop if the pass doesn't change input
(define input-vector (list->vector (string->list input)))
(string-length (react input-vector))

; Part 2
; Determine which polymer is the most reactive, then find the length after reacting just that guy



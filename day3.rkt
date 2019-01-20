#lang racket

(define lines (file->lines "input_day3.txt"))

(define (get-width claim)
    (list-ref claim 0)
)
(define (get-height claim)
    (list-ref claim 1)
)
(define (get-left claim)
    (list-ref claim 2)
)
(define (get-top claim)
    (list-ref claim 3)
)

(define (get-coords claim)
    ; Produce a set of coords (x, y) 
    ; i.e. 2x1 3,3 --> (3, 3) (3, 4) 
    (for*/list ([i (range (get-width claim))]
                [j (range (get-height claim))])
        (list (+ i (get-left claim)) (+ j (get-top claim))))
) 

(define (get-claim line) 
    ; some string parsing
    (define offset (string-split (string-trim (list-ref (string-split line) 2) ":") ","))
    (define size  (string-split (list-ref (string-split line) 3) "x"))
    (append (map string->number size) (map string->number offset))
)

(define (get-num-overlap coords)
    (define (helper coords curr-hash-table)
        (if (null? coords)
            curr-hash-table
            (helper (rest coords) (hash-update curr-hash-table (first coords) (lambda (x) (+ x 1)) 0))))
    (length (filter (lambda (x) (> x 1)) (hash-values (helper coords (make-immutable-hash '())))))
)

; This is definitely slower than it needs to be, still learning the zen of Lisp
(define (find-overlap lines) 
    (define claims (map get-claim lines))
    (define (all-coords claims)
        (for/fold ([res '()]) ([coord claims])
            (append res (get-coords coord)))
    )
    (get-num-overlap (all-coords claims))
)
(find-overlap lines)

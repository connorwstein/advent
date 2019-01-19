#lang racket

; Part 1
; Find the number of IDs which have a letter which appears exactly twice
; Find the number of IDs which have a letter which appears exactly 3 times
; Multiply those to get a checksum
(define (char-count str) 
    (define (helper char-list count-map)
        (if (null? char-list) 
            count-map
            (helper (cdr char-list) (hash-update count-map (car char-list) (lambda (x) (+ x 1)) 0)))
    )
    (helper (string->list str)  (make-immutable-hash '()))
)

(define (has-2? str)
    (present? 2 (hash-values (char-count str)))
)
(define (has-3? str)
    (present? 3 (hash-values (char-count str)))
)

(define (present? v l) 
    (cond ((empty? l) #f)
          ((eq? (car l) v) #t)
          (else (present? v (cdr l))))
)

(define (checksum in)
    (define (helper in num-2s num-3s)
        (let ((line (read-line in 'any)))
            (if (eof-object? line)
                (* num-2s num-3s)
                (helper in 
                        (if (has-2? line) (+ num-2s 1) num-2s)
                        (if (has-3? line) (+ num-3s 1) num-3s)))))
    (helper in 0 0)
)

(call-with-input-file "input_day2.txt" checksum)

; Part 2
; Find two box IDs which differ by one char


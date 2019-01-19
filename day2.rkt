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
        (let ([line (read-line in 'any)])
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
; Probs a faster way with edit-distance / rabin-karp
(define (diff-count s1 s2) 
    (define (helper char-list1 char-list2 count)
        (cond ((null? char-list1) (+ count (length char-list2)))
              ((null? char-list2) (+ count (length char-list1)))
              ((eq? (car char-list1) (car char-list2)) (helper (cdr char-list1) (cdr char-list2) count))
              (else (helper (cdr char-list1) (cdr char-list2) (+ count 1)))) 
    )
    (helper (string->list s1) (string->list s2) 0)
)

(define (get-magic s1 s2)
    (for/list ([i (string->list s1)]
               [j (string->list s2)]
                #:when (eq? i j))
        i)
)
(define (find-magic-boxes file)
    (define lines (file->lines file)) ;lines is a list
    (for*/list ([i lines]
                [j lines]
                #:when (eq? (diff-count i j) 1))
            (get-magic i j))
)
(list->string (car (find-magic-boxes "input_day2.txt")))

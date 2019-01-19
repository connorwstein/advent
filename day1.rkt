#lang racket/base

(require racket/stream)

; Day 1
(define (compute-freq file curr)
  (let ((line (read-line file 'any)))
    (if (eof-object? line)
        curr
        (compute-freq file (+ curr (string->number line))))))

(call-with-input-file "input_day1.txt" (lambda (in) (compute-freq in 0)))

(define (get-infinite-stream in filename s) 
	(let ((line (read-line in 'any)))
		(if (eof-object? line)
		 	(begin (close-input-port in) (get-infinite-stream (open-input-file filename) filename s)) ; probably a cleaner way of resetting the read pointer?
			(stream-cons (string->number line) (get-infinite-stream in filename s))))
)

(define (first-dup input-values curr-freq curr-freq-map) 
	(let ((new-freq (+ curr-freq (stream-first input-values)))) 
		(if (hash-has-key? curr-freq-map new-freq)
			 new-freq
			(first-dup (stream-rest input-values) new-freq (hash-set curr-freq-map new-freq #t))))
)

(first-dup (get-infinite-stream (open-input-file "input_day1.txt") "input_day1.txt" '()) 0 (make-immutable-hash '()))


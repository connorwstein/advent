#lang racket
(require gregor)
(define lines (file->lines "input_day4.txt"))
; Form a list of (datetime, event) tuples
(define events (for/list ([i lines])
    (let* ([fields (string-split i " ")]
           [date   (string-join (list (list-ref fields 0) 
                                      (list-ref fields 1)) " ")]
           [event  (list-ref (string-split i "] ") 1)])
        (list (parse-datetime date "[yyyy-MM-dd HH:mm]") event))
))
(define sorted-events (sort events (lambda (x y) (datetime<? (car x) (car y)))))
(define (get-guard-id event)
    (string-trim (cadr (string-split (cadr event) " ")) "#")
)
(define (get-guard-data sorted-events guard asleep guard-data)
    (define (minutes x y)
        (range (->minutes x) (+ (->minutes x) (minutes-between x y))))
    (if (null? sorted-events)
        guard-data
    (let* ([event (car sorted-events)]
          [event-type (cadr event)]
          [timestamp (car event)])
        (cond ([null? event] guard-data)
              ([string-contains? event-type "Guard"]
                    (get-guard-data (cdr sorted-events) (get-guard-id event) asleep guard-data))
              ([string-contains? event-type "asleep"]
                    (get-guard-data (cdr sorted-events) guard timestamp guard-data))
              ([string-contains? event-type "wakes"]
                    (get-guard-data (cdr sorted-events) guard timestamp
                            (hash-update guard-data guard (lambda (x) (append x (minutes asleep timestamp))) (minutes asleep timestamp))))
              (else "error"))))
)
(define guard-data (get-guard-data sorted-events (get-guard-id (car sorted-events)) '() (hash)))

; Walk through the guard data looking for the best
; This is a bit un-lisplike, devolving :(  
(define (get-sleepiest-guard guard-data)
    (define max-asleep 0)
    (define sleepiest-guard '()) 
    (for ([(k v) guard-data])
        (when (> (length v) max-asleep) 
                (begin (set! max-asleep (length v)) (set! sleepiest-guard k))))
    sleepiest-guard
)
(define sleepiest-guard (get-sleepiest-guard guard-data))
(define mins (hash-ref guard-data sleepiest-guard))

(define (get-counts input-list)
    ; return hash table of counts from a list of nums
    (for/fold ([res (hash)])
              ([i input-list])
        (hash-update res i (lambda (x) (+ x 1)) 0))
)
(define counts (hash->list (get-counts mins)))
(* (string->number sleepiest-guard) (car (argmax cdr counts)))

; Part 2
(define (all-guards-sleepiest-minute guard-data)
    ; reduce the hash table
    (define best (cons -1 -1))
    (define guard -1)
    (for ([(k v) guard-data])
        (let ([sleepiest-min (argmax cdr (hash->list (get-counts v)))]) ; BUG need the sleepiest min
            (when (> (cdr sleepiest-min) (cdr best))
                (begin (set! best sleepiest-min) (set! guard k)))))
    (* (string->number guard) (car best))
)
(all-guards-sleepiest-minute guard-data)




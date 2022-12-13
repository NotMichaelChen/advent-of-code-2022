#lang racket

(require threading)
(require "../util/util.rkt")

(struct packet (left right) #:transparent)

(define raw-input (filter (compose non-empty-string? string-trim) (file->lines "input.txt")))

(define (extract-number chars)
  (let*-values ([(num-chars rest-chars) (splitf-at chars char-numeric?)]
                [(parsed-number) (string->number (list->string num-chars))])
    (values parsed-number rest-chars)))

(define (parse-chars chars)
  (let loop ([chars (rest chars)]
             [acc null])
    (cond
      [(char=? (first chars) #\,) (loop (rest chars) acc)]
      [(char=? (first chars) #\[)
       (let-values ([(lst next-chars) (parse-chars chars)])
         (loop next-chars (cons lst acc)))]
      [(char=? (first chars) #\]) (values (reverse acc) (safe-rest chars))]
      ; assume number
      [else
       (let-values ([(num next-chars) (extract-number chars)])
         (loop next-chars (cons num acc)))])))

(define (parse-line line)
  (let-values ([(res _) (parse-chars (string->list line))]) res))

(define (parse-packet lines)
  (match-let ([(list left right) lines])
    (packet (parse-line left) (parse-line right))))

(define (compare-nums left right)
  (cond
    [(< left right) #t]
    [(> left right) #f]
    [else null]))

(define (compare-lists left right)
  (let* ([compare-res
          (for/list ([l left]
                     [r right])
            (compare-vals l r))]
         [filter-res (filter boolean? compare-res)])
    (cond
      [(not (empty? filter-res)) (first filter-res)]
      [(< (length right) (length left)) #f]
      [(> (length right) (length left)) #t]
      [else null])))

(define (compare-vals left right)
  (cond
    [(and (integer? left) (integer? right)) (compare-nums left right)]
    [(and (list? left) (list? right)) (compare-lists left right)]
    [(and (integer? left) (list? right)) (compare-lists (list left) right)]
    [(and (list? left) (integer? right)) (compare-lists left (list right))]))

(define (compare-lines left right)
  (let ([res (compare-vals left right)])
    (if (not res) #f #t))) ; turns a "null" result into "true"

(define (packet-right-order? pkt)
  (compare-lines (packet-left pkt) (packet-right pkt)))

(define part1
  (~>> raw-input
       (list-chunk _ 2)
       (map parse-packet)
       (map packet-right-order?)
       (indexes-of _ #t)
       (map add1)
       (apply +)))

(define part2
  (~>> raw-input
       (map parse-line)
       (append (list '((2)) '((6))))
       (list-zip-index)
       (sort _ (lambda (l r) (compare-lines (first l)  (first r))))
       ; I know the first two indexes are the ones I added
       (indexes-where _ (lambda (o) (<= (second o) 1)))
       (map add1)
       (apply *)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
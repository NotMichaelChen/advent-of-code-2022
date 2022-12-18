#lang racket

(require threading)
(require "../util/util.rkt")

(struct valve (name rate) #:transparent)

(define raw-input (file->lines "input.txt"))

(define (parsed-to-valves parsed-input)
  (~>> parsed-input
       (map (lambda (line-vals)
              (let ([rate (string->number (second line-vals))])
                (if (= 0 rate) #f (valve (first line-vals) rate)))))
       (filter identity)))

(define (parsed-to-adjacency parsed-input)
  (let ([adjacency-lists
         (map
          (lambda (line-vals) (list (first line-vals) (map string-trim (string-split (third line-vals) ","))))
          parsed-input)])
    (define res (make-hash))
    (for-each (lambda (row) (match-let ([(list key value) row]) (hash-set! res key value))) adjacency-lists)
    res))

(define (parse-input input)
  (let ([raw-values
         (map
          (lambda (line) (rest (regexp-match #rx"^Valve (.*) has flow rate=(.*); tunnel[s]? lead[s]? to valve[s]? (.*)$" line)))
          input)])
    (values (parsed-to-valves raw-values) (parsed-to-adjacency raw-values))))

; https://en.wikipedia.org/wiki/Floyd%E2%80%93Warshall_algorithm
(define (floyd-warshall adjacency)
  (define vertices (hash-keys adjacency))
  (define dist (make-hash))
  (define (safe-get-dist pair)
    (hash-ref dist pair +inf.0))
  ; init
  (hash-for-each
   adjacency
   (lambda (from adj-list)
     (for-each
      (lambda (to) (hash-set! dist (cons from to) 1))
      adj-list)
     (hash-set! dist (cons from from) 0))
   )
  (for* ([k vertices]
         [i vertices]
         [j vertices])
    (when
        (> (safe-get-dist (cons i j))
           (+ (safe-get-dist (cons i k)) (safe-get-dist (cons k j))))
      (hash-set! dist (cons i j) (+ (safe-get-dist (cons i k)) (safe-get-dist (cons k j))))))
  dist)

; sub 1 to turn on valve
(define (sub-time time-left from to dists)
  (- time-left (hash-ref dists (cons (valve-name from) (valve-name to))) 1))

(define (pressure-relieved from to time-left dists)
  (* (valve-rate to) (sub-time time-left from to dists)))

; https://en.wikipedia.org/wiki/Held%E2%80%93Karp_algorithm
; key: (path end-valve)
; value: (pressure time-left)
(define (held-karp dists valves initial-time)
  (define (compute-pressure-path pressure-path subset k)
    (define candidates
      (for/list ([m subset]
                 #:unless (equal? m k))
        (let* ([new-subset (set-remove subset k)]
               [lookup (hash-ref pressure-path (cons new-subset m))])
          (cons
           (+ (car lookup) (pressure-relieved m k (cdr lookup) dists))
           (sub-time (cdr lookup) m k dists)))))
    (first (sort candidates > #:key car)))
  (define start-valve (valve "AA" 0))
  (define pressure-path (make-hash))
  (for ([k valves])
    (hash-set!
     pressure-path
     (cons (set k) k)
     (cons (pressure-relieved start-valve k initial-time dists) (sub-time initial-time start-valve k dists))))
  (for* ([subset-size (inclusive-range 2 (length valves))]
         [subset (combinations valves subset-size)]
         [k subset])
    (hash-set!
     pressure-path
     (cons (list->set subset) k)
     (compute-pressure-path pressure-path (list->set subset) k)))
  pressure-path)

(define-values (valves adjacency) (parse-input raw-input))

(define dists (floyd-warshall adjacency))

(define (find-best-in-subset held-karp-paths subset)
  (let ([pressures
         (for/list ([(key value) (in-hash held-karp-paths)]
                    #:when (subset? (car key) subset))
           (car value))])
    (if (empty? pressures)
        0
        (apply max pressures))))

(define (find-best-with-split held-karp-paths)
  (define best-by-length
    (let ([res (make-hash (list (cons 0 0)))])
      (for ([(key value) (in-hash held-karp-paths)])
        (when (> (car value)
                 (hash-ref res (set-count (car key)) 0))
          (hash-set! res (set-count (car key)) (car value))))
      res))
  (define best-so-far 0)
  (for ([(key value) (in-hash held-karp-paths)])
    (let* ([my-set (car key)]
           [max-elephant-set (set-subtract (list->set valves) my-set)])
      (when (> (+ (car value) (hash-ref best-by-length (set-count max-elephant-set))) best-so-far)
        (set! best-so-far (max best-so-far
                               (+
                                (car value)
                                (find-best-in-subset held-karp-paths max-elephant-set)))))))
  best-so-far)

(define part1 (car (cdr (first (sort (hash->list (held-karp dists valves 30)) > #:key (compose car cdr))))))

(define part2 (find-best-with-split (held-karp dists valves 26)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
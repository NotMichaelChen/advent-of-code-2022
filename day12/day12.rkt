#lang racket

(require threading)
(require "../util/util.rkt")

(struct node (pos dist) #:transparent)

(define heightmap (~>> (file->lines "input.txt")
                       list->vector
                       (vector-map (compose list->vector string->list))))

(define start-pos (vec-matrix-find heightmap #\S))

(define (next-char c)
  (~> c
      char->integer
      add1
      integer->char))

(define (map-special-char c)
  (cond
    [(char=? c #\S) #\a]
    [(char=? c #\E) #\z]
    [else c]))

(define (can-go from to)
  (let ([from-c (map-special-char from)]
        [to-c (map-special-char to)])
    (or (char=? (next-char from-c) to-c)
        (char<=? to-c from-c))))

(define (valid-step heightmap visited from-pos to-pos)
  (let ([from (vec-matrix-ref heightmap from-pos)]
        [to (vec-matrix-ref heightmap to-pos)])
    (and (can-go from to)
         (not (hash-has-key? visited to-pos)))))

(define (get-node unvisited pos)
  (node pos (hash-ref unvisited pos +inf.0)))

(define (update-node new-dist nod)
  (if (< new-dist (node-dist nod))
      (node (node-pos nod) new-dist)
      nod))

(define (update-hash-node nod hash)
  (hash-set hash (node-pos nod) (node-dist nod)))

(define (hash-remove-add hash remove-key add-nodes)
  (hash-remove (foldl update-hash-node hash add-nodes) remove-key))

(define (find-smallest-node hash)
  (let ([nodes (hash-map hash (lambda (k v) (node k v)))])
    (foldl
     (lambda (next cur) (if (<= (node-dist cur) (node-dist next)) cur next))
     (first nodes)
     (rest nodes))))

(define (find-shortest-path heightmap start-pos)
  (let loop ([visited (hash)]
             [unvisited (hash start-pos 0)]
             [cur-pos start-pos])
    (if (char=? (vec-matrix-ref heightmap cur-pos) #\E)
        (hash-ref unvisited cur-pos)
        (let* ([cur-dist (hash-ref unvisited cur-pos)]
               [next-dist (add1 cur-dist)]
               [unvisited-raw (vec-matrix-four-neighbors heightmap cur-pos)]
               [unvisited-pos (filter (curry valid-step heightmap visited cur-pos) unvisited-raw)]
               [nodes (map (curry get-node unvisited) unvisited-pos)]
               [updated-nodes (map (curry update-node next-dist) nodes)]
               [updated-unvisited (hash-remove-add unvisited cur-pos updated-nodes)])
          (if (hash-empty? updated-unvisited)
              +inf.0
              (loop (hash-set visited cur-pos cur-dist)
                    updated-unvisited
                    (node-pos (find-smallest-node updated-unvisited))))))))

(define (find-all-indices matrix val)
  (define row-count (vector-length matrix))
  (define col-count (vector-length (vector-ref matrix 0)))
  (for*/list ([r (range row-count)]
              [c (range col-count)]
              #:when (equal? val (vec-matrix-ref matrix (posn r c))))
    (posn r c)))

(define part1 (find-shortest-path heightmap start-pos))

(define part2
  (~>> (cons start-pos (find-all-indices heightmap #\a))
       (map (curry find-shortest-path heightmap))
       (apply min)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
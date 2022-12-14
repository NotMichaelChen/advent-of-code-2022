#lang racket

(require threading)
(require "../util/util.rkt")

(define raw-input (file->lines "input.txt"))

(define (pair-to-posn str-pair)
  (match-let ([(list col row) (string-split str-pair ",")])
    (posn (string->number row) (string->number col))))

(define (segment-to-points pnts)
  (let ([row-lo (min (posn-row (first pnts)) (posn-row (second pnts)))]
        [row-hi (max (posn-row (first pnts)) (posn-row (second pnts)))]
        [col-lo (min (posn-col (first pnts)) (posn-col (second pnts)))]
        [col-hi (max (posn-col (first pnts)) (posn-col (second pnts)))])
    (list->set
     (if (= row-lo row-hi)
         (map
          (lambda (c) (posn row-lo c))
          (inclusive-range col-lo col-hi))
         ; assume columns are equal
         (map
          (lambda (r) (posn r col-lo))
          (inclusive-range row-lo row-hi))))))

(define (parse-line line)
  (~>> line
       (string-split _ " -> ")
       (map pair-to-posn)
       (list-window _ 2)
       (map segment-to-points)
       (apply set-union)))

(define (find-largest-row wall-pnts)
  (~>> wall-pnts
       set->list
       (map posn-row)
       (apply max)))

(define (sand-valid-pos? pos wall-pnts all-sand bottom-row)
  (and (not (= (posn-row pos) bottom-row))
       (not (set-member? all-sand pos))
       (not (set-member? wall-pnts pos))))

(define (sand-next-poses pos)
  (list
   (struct-copy posn pos [row (add1 (posn-row pos))])
   (posn (add1 (posn-row pos)) (sub1 (posn-col pos)))
   (posn (add1 (posn-row pos)) (add1 (posn-col pos)))))

(define (sand-move pos wall-pnts all-sand bottom-row)
  (define valid-moves
    (for/list ([next-pos (sand-next-poses pos)]
               #:when (sand-valid-pos? next-pos wall-pnts all-sand bottom-row))
      next-pos))
  (if (empty? valid-moves)
      pos
      (first valid-moves)))

(define (simulate-sand wall-pnts all-sand bottom-row)
  (let loop ([sand (posn 0 500)])
    (let ([next-sand (sand-move sand wall-pnts all-sand bottom-row)])
      (if (equal? sand next-sand)
          sand
          (loop next-sand)))))

(define (simulate-sands wall-pnts has-floor)
  (define bottom-row (+ 2 (find-largest-row wall-pnts)))
  (define break-cond
    (if has-floor
        (lambda (next-sand) (equal? next-sand (posn 0 500)))
        (lambda (next-sand) (= (sub1 bottom-row) (posn-row next-sand)))))
  (let loop ([all-sand (set)])
    (let ([next-sand (simulate-sand wall-pnts all-sand bottom-row)])
      (if (break-cond next-sand)
          all-sand
          (loop (set-add all-sand next-sand))))))

(define wall-pnts
  (~>> raw-input
       (map parse-line)
       (apply set-union)))

(define part1 (set-count (simulate-sands wall-pnts #f)))

; Add 1 for missing point at (500,0) since we return before adding it to the final set
(define part2 (add1 (set-count (simulate-sands wall-pnts #t))))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
#lang racket

(require threading)
(require "../util/util.rkt")

(define raw-input (file->lines "input.txt"))

(define (parse-sack line)
  (define middle
    (quotient (string-length line) 2))
  (define first
    (substring line 0 middle))
  (define second
    (substring line middle (string-length line)))
  (list first second))

(define (priority item)
  (define item-num (char->integer item))
  (if (char-lower-case? item)
      (+ 1 (- item-num 97))
      (+ 27 (- item-num 65))))

(define (find-common-item lst)
  (~>> lst
       (map string->set)
       (apply set-intersect)
       set-first))

(define part1
  (~>> raw-input
       (map parse-sack)
       (map find-common-item)
       (map priority)
       (apply +)))

(define part2
  (~>> raw-input
       (list-chunk _ 3)
       (map find-common-item)
       (map priority)
       (apply +)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
#lang racket

(require threading)
(require "../util/util.rkt")

(define raw-input (file->lines "input.txt"))

(define (parse-line line)
  (~> line
      (string-split ",")
      (map (lambda (pair) (string-split pair "-")) _)))

(define (contains inner outer)
  (and
   (>= (string->number (first inner)) (string->number (first outer)))
   (<= (string->number (second inner)) (string->number (second outer)))))

(define (overlaps lower upper)
  (and
   (>= (string->number (second lower)) (string->number (first upper)))
   (<= (string->number (second lower)) (string->number (second upper)))))

(define part1
  (~>> raw-input
       (map parse-line)
       (map (lambda (pairs)
              (or
               (contains (first pairs) (second pairs))
               (contains (second pairs) (first pairs)))))
       (filter identity)
       (length)))

(define part2
  (~>> raw-input
       (map parse-line)
       (map (lambda (pairs)
              (or
               (overlaps (first pairs) (second pairs))
               (overlaps (second pairs) (first pairs)))))
       (filter identity)
       (length)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
#lang racket

(define raw-input (file->lines "input.txt"))

(define stream (string->list (first raw-input)))

(define (contains-unique lst)
  (= (length lst) (set-count (list->set lst))))

(define (find-marker stream index marker-size)
  (if (contains-unique (take stream marker-size))
      (+ index marker-size)
      (find-marker (rest stream) (+ index 1) marker-size)))

(define part1 (find-marker stream 0 4))
(define part2 (find-marker stream 0 14))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
#lang racket

(define raw-input (port->string (open-input-file "input.txt") #:close? #t))

(define cleaned-input
  (list->string
   (filter
    (lambda (c) (not (char-blank? c)))
    (string->list raw-input))))

(define separated (map string-split (string-split cleaned-input "\n\n")))

(define numbers (map (lambda (l) (map string->number l)) separated))

(define sums (map (lambda (l) (apply + l)) numbers))

(define sorted-sums (sort sums >))

(printf "part 1: ~a\n" (take sorted-sums 1))
(printf "part 2: ~a" (apply + (take sorted-sums 3)))

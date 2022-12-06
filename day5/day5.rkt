#lang racket

(require threading)
(require "../util/util.rkt")
(require racket/match)

(struct move (amount from to) #:transparent)

(define raw-input (file->lines "input.txt"))

(define-values (stacks-input moves-input)
  (splitf-at raw-input (lambda (line) (non-empty-string? (string-trim line)))))

(define (parse-stacks stacks-input)
  (define (parse-raw-crate crate)
    (~>> crate
         (list->string)
         (string-trim)))

  (define (parse-stack-line line)
    (~>> line
         (string->list)
         (list-chunk _ 4)
         (map parse-raw-crate)))

  (define (drop-empty-crates stack)
    (dropf stack (lambda (crate)
                   (string=? crate ""))))

  (~>> stacks-input
       (drop-right _ 1)
       (map parse-stack-line)
       transpose
       (map drop-empty-crates)))

; TODO replace with string-match macro
(define (parse-move line)
  (match line
    [(regexp #rx"^move (.*) from (.*) to (.*)$" (list _ amount from to))
     (move (string->number amount) (string->number from) (string->number to))]))

(define (apply-move stacks move-instance should-extract-reverse)
  (define from (- (move-from move-instance) 1))
  (define to (- (move-to move-instance) 1))
  (define raw-extracted
    (take (list-ref stacks from) (move-amount move-instance)))
  (define extracted
    (if should-extract-reverse
        (reverse raw-extracted)
        raw-extracted))
  (define without-extracted
    (list-update stacks from (lambda (stack) (drop stack (move-amount move-instance)))))
  (list-update without-extracted to (lambda (stack) (append extracted stack))))

(define (apply-moves stacks moves should-extract-reverse)
  (if (empty? moves)
      stacks
      (apply-moves (apply-move stacks (first moves) should-extract-reverse) (rest moves) should-extract-reverse)))

(define (format-output raw-stacks)
  (~>> raw-stacks
       (map first)
       (map string->list)
       (map second)
       (list->string)))

(define parsed-moves
  (~>> (rest moves-input)
       (map parse-move)))

(define parsed-stacks
  (parse-stacks stacks-input))

(define part1
  (~>> (apply-moves parsed-stacks parsed-moves #t)
       (format-output)))

(define part2
  (~>> (apply-moves parsed-stacks parsed-moves #f)
       (format-output)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
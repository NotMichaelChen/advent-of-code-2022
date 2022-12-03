#lang racket

(require threading)
(require racket/match)

(struct round-hand (opponent mine) #:transparent)

(define hand-mapping (hash "X" "A" "Y" "B" "Z" "C"))

(define raw-input (file->lines "input.txt"))

(define (parse-round raw-round-str)
  (define round-chars
    (~> raw-round-str
        (string-split " ")))
  (round-hand
   (first round-chars)
   (hash-ref hand-mapping (second round-chars))))

(define win-mapping (hash "A" "B" "B" "C" "C" "A"))
(define lose-mapping (hash "A" "C" "B" "A" "C" "B"))

(define (score-selection selection)
  (case selection
    [("A") 1]
    [("B") 2]
    [("C") 3]))

(define (score-round round)
  (define selection-score
    (score-selection (round-hand-mine round)))
  (define result-score
    (cond
      [(string=? (hash-ref win-mapping (round-hand-opponent round)) (round-hand-mine round)) 6]
      [(string=? (round-hand-opponent round) (round-hand-mine round)) 3]
      [else 0]))
  (+ selection-score result-score))

(define (part-two-map-to-mine round)
  (case (round-hand-mine round)
    [("A") (hash-ref lose-mapping (round-hand-opponent round))]
    [("B") (round-hand-opponent round)]
    [("C") (hash-ref win-mapping (round-hand-opponent round))]))

(define (score-round-two round)
  (define selection-score
    (score-selection (part-two-map-to-mine round)))
  (define result-score
    (case (round-hand-mine round)
      [("A") 0]
      [("B") 3]
      [("C") 6]))
  (+ selection-score result-score))

(define parsed-input (map parse-round raw-input))

(define part1
  (~>> parsed-input
       (map score-round)
       (apply +)))

(define part2
  (~>> parsed-input
       (map score-round-two)
       (apply +)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
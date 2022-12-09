#lang racket

(require threading)

(struct posn (row col) #:transparent)

(define (posn-build-row pos nums)
  (map (lambda (n) (struct-copy posn pos [row n])) nums))

(define (posn-build-col pos nums)
  (map (lambda (n) (struct-copy posn pos [col n])) nums))

(define (posn-dist-greater-one? left right)
  (or (> (abs (- (posn-row left) (posn-row right))) 1)
      (> (abs (- (posn-col left) (posn-col right))) 1)))

(define (posn-shares-dimension? left right)
  (or (= (posn-row left) (posn-row right))
      (= (posn-col left) (posn-col right))))

(define (posn-move-cardinal from to)
  (cond
    [(> (posn-row to) (posn-row from)) (struct-copy posn from [row (add1 (posn-row from))])]
    [(< (posn-row to) (posn-row from)) (struct-copy posn from [row (sub1 (posn-row from))])]
    [(> (posn-col to) (posn-col from)) (struct-copy posn from [col (add1 (posn-col from))])]
    [(< (posn-col to) (posn-col from)) (struct-copy posn from [col (sub1 (posn-col from))])]))

(define (posn-move-diagonal from to)
  (define r (posn-row from))
  (define c (posn-col from))
  (cond
    [(and (> (posn-row to) (posn-row from)) (> (posn-col to) (posn-col from)))
     (posn (add1 r) (add1 c))]
    [(and (> (posn-row to) (posn-row from)) (< (posn-col to) (posn-col from)))
     (posn (add1 r) (sub1 c))]
    [(and (< (posn-row to) (posn-row from)) (> (posn-col to) (posn-col from)))
     (posn (sub1 r) (add1 c))]
    [(and (< (posn-row to) (posn-row from)) (< (posn-col to) (posn-col from)))
     (posn (sub1 r) (sub1 c))]))

(struct motion (dir amnt) #:transparent)

(struct state (head tails hist) #:transparent)

(define raw-input (file->lines "input.txt"))

(define (parse-motion line)
  (let* ([splited (string-split line)]
         [dir (first splited)]
         [parsed-amt (string->number (second splited))])
    (motion dir parsed-amt)))

(define (produce-positions pos mot)
  (case (motion-dir mot)
    [("U") (posn-build-row pos (inclusive-range (add1 (posn-row pos)) (+ (posn-row pos) (motion-amnt mot))))]
    [("D") (posn-build-row pos (inclusive-range (sub1 (posn-row pos)) (- (posn-row pos) (motion-amnt mot)) -1))]
    [("L") (posn-build-col pos (inclusive-range (sub1 (posn-col pos)) (- (posn-col pos) (motion-amnt mot)) -1))]
    [("R") (posn-build-col pos (inclusive-range (add1 (posn-col pos)) (+ (posn-col pos) (motion-amnt mot))))]))

(define (update-tail tail new-head)
  (cond
    [(not (posn-dist-greater-one? tail new-head)) tail]
    [(posn-shares-dimension? tail new-head) (posn-move-cardinal tail new-head)]
    [else (posn-move-diagonal tail new-head)]))

(define (update-tails tails new-head)
  (let loop ([tails tails]
             [head new-head])
    (if (empty? tails)
        null
        (let ([new-tail (update-tail (first tails) head)])
          (cons new-tail (loop (rest tails) new-tail))))))

(define (update-state new-head ste)
  (let ([new-tails (update-tails (state-tails ste) new-head)])
    (state new-head new-tails (set-add (state-hist ste) (last new-tails)))))

(define (apply-motion mot ste)
  (foldl update-state ste (produce-positions (state-head ste) mot)))

(define (apply-motions ste mots)
  (foldl apply-motion ste mots))

(define motions (map parse-motion raw-input))

(define part1
  (~>> motions
   (apply-motions (state (posn 0 0) (list (posn 0 0)) (set)))
   state-hist
   set-count))

(define part2
  (~>> motions
       (apply-motions (state (posn 0 0) (make-list 9 (posn 0 0)) (set)))
       state-hist
       set-count))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
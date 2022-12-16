#lang racket

(require threading)
(require "../util/util.rkt")

(define input-filename "input.txt")
(define raw-input (file->lines input-filename))

(define (string-posn row-s col-s)
  (posn (string->number row-s) (string->number col-s)))

(define (parse-line line)
  (match-let ([(list _ sens-col sens-row beac-col beac-row)
               (regexp-match #rx"^Sensor at x=(.*), y=(.*): closest beacon is at x=(.*), y=(.*)$" line)])
    (list (string-posn sens-row sens-col) (string-posn beac-row beac-col))))

(define (man-dist left right)
  (+ (abs (- (posn-row left) (posn-row right)))
     (abs (- (posn-col left) (posn-col right)))))

(define (covers-range-row sensor radius row)
  (let* ([row-radius (abs (- (posn-row sensor) row))]
         [inv-row-radius (- radius row-radius)])
    (list (- (posn-col sensor) inv-row-radius)
          (+ (posn-col sensor) inv-row-radius))))

; inclusive range
(define (covered-range sensor beacon row)
  (let* ([radius (man-dist sensor beacon)]
         [upper-row (+ (posn-row sensor) radius)]
         [lower-row (- (posn-row sensor) radius)])
    (if (<= lower-row row upper-row)
        (covers-range-row sensor radius row)
        #f)))

(define (attempt-merge-range lower upper)
  ; ranges disjointed by 1 are still connected
  (if (< (add1 (second lower)) (first upper))
      (list upper lower)
      (list (list (min (first lower) (first upper))
                  (max (second lower) (second upper))))))

(define (merge-ranges ranges)
  (foldl
   (lambda (cur-range acc-ranges) (append (attempt-merge-range (first acc-ranges) cur-range) (safe-rest acc-ranges)))
   (list (first ranges))
   (rest ranges)))

(define (range-less-than? left right)
  (if (= (first left) (first right))
      (< (second left) (second right))
      (< (first left) (first right))))

(define (cap-range cap rnge)
  (if (or (< (second rnge) 0)
          (> (first rnge) cap))
      #f
      (list (max 0 (first rnge)) (min cap (second rnge)))))

(define (occupied-range sensor-beacons cap row)
  (~>> sensor-beacons
       (map (lambda (pair) (covered-range (first pair) (second pair) row)))
       (filter identity)
       (map (curry cap-range par2-limit))
       (filter identity)
       (sort _ range-less-than?)
       merge-ranges))

(define (find-distress-signal sensor-beacons cap)
  (define row-with-ranges
    (let loop ([row 0])
      (let ([range-val (occupied-range sensor-beacons cap row)])
        (if (> (length range-val) 1)
            (list row range-val)
            (loop (add1 row))))))
  (posn (first row-with-ranges) (sub1 (first (first (second row-with-ranges))))))

(define (compute-tuning-frequency pos)
  (+ (posn-row pos) (* (posn-col pos) 4000000)))

(define part1-row
  (if (string=? input-filename "test.txt") 20 2000000))

(define par2-limit
  (if (string=? input-filename "test.txt") 20 4000000))

(define parsed-input (map parse-line raw-input))

(define part1
  (~>> parsed-input
       (map (lambda (pair) (covered-range (first pair) (second pair) part1-row)))
       (filter identity)
       (sort _ range-less-than?)
       merge-ranges
       (map (lambda (range) (- (second range) (first range))))
       (apply +)))

(define part2
  (~> parsed-input
      (find-distress-signal par2-limit)
      (compute-tuning-frequency)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
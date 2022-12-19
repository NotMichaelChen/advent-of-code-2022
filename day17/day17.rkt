#lang racket

; I am not proud of the racket code contained within

(require threading)
(require "../util/util.rkt")

(define raw-input (file->lines "input.txt"))

(define dirs-vec
  (~>> raw-input
       first
       string->list
       list->vector))

(define (shape-next shape)
  (match shape
    [#\- #\+]
    [#\+ #\L]
    [#\L #\I]
    [#\I #\O]
    [#\O #\-]))

; highest refers to first empty row from floor, or zero if at floor
(define (shape-spawn highest shape)
  (define bottom (+ 3 highest))
  (match shape
    [#\- (list (posn bottom 2) (posn bottom 3) (posn bottom 4) (posn bottom 5))]
    [#\+ (list (posn bottom 3) (posn (+ 1 bottom) 3) (posn (+ 2 bottom) 3) (posn (+ 1 bottom) 2) (posn (+ 1 bottom) 4))]
    [#\L (list (posn bottom 2) (posn bottom 3) (posn bottom 4) (posn (+ 1 bottom) 4) (posn (+ 2 bottom) 4))]
    [#\I (list (posn bottom 2) (posn (+ 1 bottom) 2) (posn (+ 2 bottom) 2) (posn (+ 3 bottom) 2))]
    [#\O (list (posn bottom 2) (posn bottom 3) (posn (+ 1 bottom) 2) (posn (+ 1 bottom) 3))]))

(define (valid-pos? occupied ignore-floor pos)
  (and (>= (posn-col pos) 0)
       (<= (posn-col pos) 6)
       (or ignore-floor (>= (posn-row pos) 0))
       (not (set-member? occupied pos))))

(define (shape-valid? occupied shape ignore-floor)
  (~>> shape
       (map (curry valid-pos? occupied ignore-floor))
       (filter not)
       empty?))

(define (shape-move occupied shape dir ignore-floor)
  (let ([next-shape
         (match dir
           [#\< (map (lambda (pos) (posn-add-col pos -1)) shape)]
           [#\> (map (lambda (pos) (posn-add-col pos 1)) shape)])])
    (if (shape-valid? occupied next-shape ignore-floor)
        next-shape
        shape)))

(define (shape-move-down occupied shape ignore-floor)
  (let ([next-shape
         (map (lambda (pos) (posn-add-row pos -1)) shape)])
    (if (shape-valid? occupied next-shape ignore-floor)
        (list next-shape #t)
        (list shape #f))))

; returns next empty row above shape
(define (next-empty-row shape)
  (add1 (apply max (map posn-row shape))))

; wtf is this
(define (cut-occupied occupied height)
  (define diff 30) ; ???
  (list->set
   (filter (lambda (pos) (<= (abs (- height (posn-row pos))) diff)) (set->list occupied))))

(define (simulate-shapes
         dirs
         max-rocks
         [init-occupied (set)]
         [init-shape #\-]
         [init-shape-posns (shape-spawn 0 #\-)]
         [ignore-floor #f])
  (define (wrap-inc i)
    (remainder (add1 i) (vector-length dirs)))
  (define loop-start-states '())
  (let loop ([cur-shape init-shape]
             [shape-posns init-shape-posns]
             [occupied init-occupied]
             [dirs-index 0]
             [height 0]
             [rocks-dropped 0])
    (if
     (= rocks-dropped max-rocks)
     height
     (match-let* ([moved (shape-move occupied shape-posns (vector-ref dirs dirs-index) ignore-floor)]
                  [(list moved-down valid) (shape-move-down occupied moved ignore-floor)])
       (if valid
           (loop cur-shape moved-down occupied (wrap-inc dirs-index) height rocks-dropped)
           (let* ([new-height (max height (next-empty-row moved-down))]
                  [new-occupied (set-union occupied (list->set moved-down))]
                  [next-shape (shape-next cur-shape)]
                  [new-shape-posns (shape-spawn new-height next-shape)]
                  [new-rocks-dropped (add1 rocks-dropped)])
             (loop next-shape new-shape-posns new-occupied (wrap-inc dirs-index) new-height new-rocks-dropped)))))))

; everything normalized to height=0
(struct loop-start-state (cur-shape shape-posns height rocks-dropped height-diff) #:transparent)

; ???
(define (find-loop dirs)
  (define (find-index-previous? loop-start-states next-start-state)
    (index-where
     loop-start-states
     (lambda (loop-state)
       (and
        (char=? (loop-start-state-cur-shape loop-state)  (loop-start-state-cur-shape next-start-state))
        (equal? (loop-start-state-shape-posns loop-state) (loop-start-state-shape-posns next-start-state))
        (= (loop-start-state-height-diff loop-state) (loop-start-state-height-diff next-start-state))))))
  (define (normalize-posns posns height)
    (map (lambda (pos) (posn-add-row pos (- height))) posns))
  (define (wrap-inc i)
    (remainder (add1 i) (vector-length dirs)))
  (define (height-diff start-state height)
    (- height (loop-start-state-height start-state)))
  (define loop-start-states
    (list (loop-start-state #\- (shape-spawn 0 #\-) 0 0 0)))
  (let loop ([cur-shape #\-]
             [shape-posns (shape-spawn 0 #\-)]
             [occupied (set)]
             [dirs-index 0]
             [height 0]
             [rocks-dropped 0])
    (when
        (and (= dirs-index 0) (not (= rocks-dropped 0)))
      (set!
       loop-start-states
       (cons (loop-start-state
              cur-shape
              (normalize-posns shape-posns height)
              height
              rocks-dropped
              (height-diff (first loop-start-states) height))
             loop-start-states)))
    (define maybe-index-prev
      (if (= dirs-index 0)
          (find-index-previous? (safe-rest loop-start-states) (first loop-start-states))
          #f))
    (if maybe-index-prev
        (list
         loop-start-states
         (add1 maybe-index-prev)
         (list->set (normalize-posns (set->list (cut-occupied occupied height)) height)))
        (match-let* ([moved (shape-move occupied shape-posns (vector-ref dirs dirs-index) #f)]
                     [(list moved-down valid) (shape-move-down occupied moved #f)])
          (if valid
              (loop cur-shape moved-down occupied (wrap-inc dirs-index) height rocks-dropped)
              (let* ([new-height (max height (next-empty-row moved-down))]
                     [new-occupied (set-union occupied (list->set moved-down))]
                     [next-shape (shape-next cur-shape)]
                     [new-shape-posns (shape-spawn new-height next-shape)]
                     [new-rocks-dropped (add1 rocks-dropped)])
                (loop next-shape new-shape-posns new-occupied (wrap-inc dirs-index) new-height new-rocks-dropped)))))))

(define (tera-drop-rocks loop-start-states begin-loop-index occupied dirs)
  ; state before loop begins
  (define state-before-loop
    (list-ref loop-start-states begin-loop-index))
  (define height-before-loop
    (loop-start-state-height state-before-loop))
  (define rocks-before-loop
    (loop-start-state-rocks-dropped state-before-loop))
  ; how much is gained per loop
  (define height-per-loop
    (- (loop-start-state-height (first loop-start-states)) height-before-loop))
  (define rocks-per-loop
    (- (loop-start-state-rocks-dropped (first loop-start-states)) rocks-before-loop))
  (define possible-loops
    (sub1 (ceiling (/ (- 1000000000000 rocks-before-loop) rocks-per-loop))))
  (define rocks-to-drop
    (- 1000000000000 (+ rocks-before-loop (* possible-loops rocks-per-loop))))
  (define remaining-height (simulate-shapes
                            dirs
                            rocks-to-drop
                            occupied
                            (loop-start-state-cur-shape (first loop-start-states))
                            (loop-start-state-shape-posns (first loop-start-states))
                            #t))
  (define total-height (+ height-before-loop (* possible-loops height-per-loop) remaining-height))
  total-height)

(define part1 (simulate-shapes dirs-vec 2022))

(define part2
  (match-let ([(list loop-start-states begin-loop-index occupied) (find-loop dirs-vec)])
    (tera-drop-rocks loop-start-states begin-loop-index occupied dirs-vec)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
#lang racket

(require threading)
(require "../util/util.rkt")

(define raw-input (file->lines "input.txt"))

(define (expand-cycles-line line x)
  (cond
    [(string-prefix? line "noop") (list x)]
    ; assume addx
    [else (let ([new-x (~>> line
                            string-split
                            second
                            string->number)])
            (list x (+ new-x x)))]))

(define (expand-cycles input)
  (let loop ([x 1]
             [input input])
    (if (empty? input)
        null
        (let* ([cycles (expand-cycles-line (first input) x)]
               [new-x (last cycles)])
          (append cycles (loop new-x (rest input)))))))

(define (pixel-at reg cycle)
  (let ([hor (modulo (sub1 cycle) 40)])
    (if (< (abs (- reg hor)) 2)
        #\#
        #\.)))

; represent value in middle of cycle
(define regs
  (append '(1) (expand-cycles raw-input)))

(define regs-with-cycles (list-zip regs (~>> regs
                                             length
                                             add1
                                             (range 1))))

(define pixels
  (map (lambda (reg-cycle) (apply pixel-at reg-cycle)) regs-with-cycles))

(define picture-lines
  (~> pixels
      (list-chunk 40)
      (take 6)
      (add-between (list #\newline))
      (flatten)
      (list->string)))

; sub1 due to 0-based indexing
(define part1
  (+ (* (list-ref regs 19) 20)
     (* (list-ref regs 59) 60)
     (* (list-ref regs 99) 100)
     (* (list-ref regs 139) 140)
     (* (list-ref regs 179) 180)
     (* (list-ref regs 219) 220)))

(define part2 picture-lines)

(printf "part 1: ~a\n" part1)
(printf "part 2: ~n~a\n" part2)
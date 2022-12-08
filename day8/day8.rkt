#lang racket

(require threading)
(require "../util/util.rkt")

(struct tree (height score) #:transparent)

(define (tree-inc input-tree)
  (tree (tree-height input-tree) (+ 1 (tree-score input-tree))))

(define (tree-visible? input-tree)
  (> (tree-score input-tree) 0))

(define raw-input (file->lines "input.txt"))

(define (string-to-trees str init-score)
  (~>> str
       string->list
       (map string)
       (map (lambda (digit) (tree (string->number digit) init-score)))))

(define (parse-trees input init-score)
  (map (lambda (str) (string-to-trees str init-score)) input))

(define part1-trees (parse-trees raw-input 0))

(define part2-trees (parse-trees raw-input 1))

(define (compute-visibility-list tree-list)
  (define (loop lst highest-height)
    (cond
      [(empty? lst) lst]
      [(<= (tree-height (first lst)) highest-height)
       (cons (first lst) (loop (rest lst) highest-height))]
      [else
       (cons (tree-inc (first lst)) (loop (rest lst) (tree-height (first lst))))]))
  (~> tree-list
      (loop -1)
      reverse
      (loop -1)
      reverse))

(define (compute-visibility trees)
  (~>> trees
       (map compute-visibility-list)
       transpose
       (map compute-visibility-list)
       transpose))

(define (count-visible trees)
  (define (bool->int bool) (if bool 1 0))
  (~>> trees
       (matrix-map (lambda (tree-instance) (bool->int (tree-visible? tree-instance))))
       (matrix-apply +)))

(define (compute-scenic-score tree-list index)
  (let* ([main-tree (list-ref tree-list index)]
        [left (take tree-list index)]
        [right (drop tree-list (+ index 1))]
        [left-filtered
         (takef-until (reverse left) (lambda (tree-instance) (< (tree-height tree-instance) (tree-height main-tree))))]
        [right-filtered
         (takef-until right (lambda (tree-instance) (< (tree-height tree-instance) (tree-height main-tree))))]
        [final-score (* (length left-filtered) (length right-filtered))]
        [final-tree (struct-copy tree main-tree [score (* final-score (tree-score main-tree))])])
    final-tree))

(define (compute-scenic-scores-list tree-list)
  (for/list ([i (range (length tree-list))])
    (compute-scenic-score tree-list i)))
  
(define (compute-scenic-scores trees)
    (~>> trees
       (map compute-scenic-scores-list)
       transpose
       (map compute-scenic-scores-list)
       transpose))

(define (highest-score trees)
  (~>> trees
       (matrix-map tree-score)
       (matrix-apply max)))

(define part1 (count-visible (compute-visibility part1-trees)))
(define part2 (highest-score (compute-scenic-scores part2-trees)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
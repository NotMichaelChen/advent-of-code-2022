#lang racket

(struct posn (row col) #:transparent)

(define (posn-add-row pos n)
  (struct-copy posn pos [row (+ (posn-row pos) n)]))

(define (posn-add-col pos n)
  (struct-copy posn pos [col (+ (posn-col pos) n)]))

(define (safe-rest lst)
  (if (empty? lst)
      null
      (rest lst)))

(define (take-up-to lst n)
  (if (< (length lst) n)
      lst
      (take lst n)))

(define (drop-up-to lst n)
  (if (< (length lst) n)
      null
      (drop lst n)))

(define (list-chunk lst n)
  (if (not (empty? lst))
      (cons (take-up-to lst n) (list-chunk (drop-up-to lst n) n))
      null))

(define (list-window lst n)
  (reverse
   (let loop ([acc '()]
              [lst lst])
     (if (< (length lst) n)
         acc
         (loop (cons (take lst n) acc) (rest lst))))))

(define (list-zip lst1 lst2)
  (map list lst1 lst2))

(define (list-zip-index lst)
  (map list lst (range (length lst))))

(define (takef-until lst pred)
  (let-values ([(left right) (splitf-at lst pred)])
    (if (empty? right)
        left
        (append left (list (first right))))))

(define (string->set str)
  (list->set (string->list str)))

(define (transpose xss)
  (apply map list xss))

(define (print-return obj)
  (println obj)
  obj)

(define (println* . vals)
  (for-each println vals))

(define (vector-where vec proc)
  (let loop ([index 0])
    (cond
      [(>= index (vector-length vec)) #f]
      [(proc (vector-ref vec index)) index]
      [else (loop (add1 index))])))

(define (matrix-map proc matrix)
  (map (lambda (lst) (map (lambda (elem) (proc elem)) lst)) matrix))

(define (matrix-apply proc matrix)
  (apply proc (map (lambda (lst) (apply proc lst)) matrix)))

(define (vec-matrix-map proc matrix)
  (vector-map (lambda (vec) (vector-map (lambda (elem) (proc elem)) vec)) matrix))

(define (vec-matrix-ref matrix pos)
  (vector-ref (vector-ref matrix (posn-row pos)) (posn-col pos)))

; TODO can this avoid looking through the entire matrix every time?
(define (vec-matrix-find matrix elem)
  (let* ([find-col (vector-map (curry vector-member elem) matrix)]
         [row-ind (vector-where find-col identity)])
    (if (equal? row-ind #f)
        #f
        (posn row-ind (vector-ref find-col row-ind)))))

(define (vec-matrix-four-neighbors matrix pos)
  (define row-count (vector-length matrix))
  (define col-count (vector-length (vector-ref matrix 0)))
  (define (invalid-pos pos)
    (or (< (posn-row pos) 0)
        (< (posn-col pos) 0)
        (>= (posn-row pos) row-count)
        (>= (posn-col pos) col-count)))
  (if (or (>= (posn-row pos) row-count)
          (>= (posn-col pos) col-count))
      null
      (let* ([r (posn-row pos)]
             [c (posn-col pos)]
             [all-neighbors
              (list (posn (add1 r) c)
                    (posn (sub1 r) c)
                    (posn r (add1 c))
                    (posn r (sub1 c)))])
        (filter-not invalid-pos all-neighbors))))

(provide (all-defined-out))
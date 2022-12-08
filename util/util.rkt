#lang racket

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

(define (string->set str)
  (list->set (string->list str)))

(define (transpose xss)
  (apply map list xss))

(define (print-return obj)
  (println obj)
  obj)

(define (takef-until lst pred)
  (let-values ([(left right) (splitf-at lst pred)])
    (if (empty? right)
        left
        (append left (list (first right))))))

(define (matrix-map proc matrix)
  (map (lambda (lst) (map (lambda (elem) (proc elem)) lst)) matrix))

(define (matrix-apply proc matrix)
  (apply proc (map (lambda (lst) (apply proc lst)) matrix)))

(provide (all-defined-out))
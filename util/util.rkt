#lang racket

(require racket/match)

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

(provide (all-defined-out))
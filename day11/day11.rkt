#lang racket

(require threading)
(require "../util/util.rkt")

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

; items stored in reverse
(struct monkey (index items op test inspect-cnt div-by) #:transparent)

(define (monkey-add-item monkey-inst item)
  (struct-copy monkey monkey-inst [items (cons item (monkey-items monkey-inst))]))

(define (monkey-clear-items monkey-inst)
  (struct-copy monkey monkey-inst
               [items null]
               [inspect-cnt (+ (monkey-inspect-cnt monkey-inst) (length (monkey-items monkey-inst)))]))

(struct throw (val to) #:transparent)

(define raw-input (file->lines "input.txt"))

(define (parse-items items)
  (~>> items
       (string-split _ ",")
       (map string-trim)
       (map string->number)
       reverse))

(define (parse-operation raw-op div-three)
  (match-let* ([(list left op-str right) (string-split raw-op)]
               [op (if (string=? op-str "+") '+ '*)]
               [left-num (string->number left)]
               [right-num (string->number right)]
               [left-symb (if (not left-num) 'n left-num)]
               [right-symb (if (not right-num) 'n right-num)]
               [base-op-symbs (list op left-symb right-symb)])
    (eval
     (if div-three
         (quasiquote (lambda (n) (quotient ((unquote-splicing base-op-symbs)) 3)))
         (quasiquote (lambda (n) ((unquote-splicing base-op-symbs)))))
     ns)))

(define (parse-test div-by true-case false-case)
  (let ([parsed-div-by (string->number div-by)]
        [parsed-true (string->number true-case)]
        [parsed-false (string->number false-case)])
    (lambda (n) (if (= 0 (remainder n parsed-div-by)) parsed-true parsed-false))))

(define (parse-monkey lines div-three)
  (match-let ([(list _ index) (regexp-match #rx"^Monkey (.*):$" (first lines))]
              [(list _ raw-items) (regexp-match #rx"^Starting items: (.*)$" (second lines))]
              [(list _ raw-op) (regexp-match #rx"^Operation: new = (.*)$" (third lines))]
              [(list _ div-by) (regexp-match #rx"^Test: divisible by (.*)$" (fourth lines))]
              [(list _ true-case) (regexp-match #rx"^If true: throw to monkey (.*)$" (fifth lines))]
              [(list _ false-case) (regexp-match #rx"^If false: throw to monkey (.*)$" (sixth lines))])
    (monkey
     (string->number index)
     (parse-items raw-items)
     (parse-operation raw-op div-three)
     (parse-test div-by true-case false-case)
     0
     (string->number div-by))))

(define (take-turn monkey-inst mod)
  (define (produce-throw item)
    (let* ([new-item-val (modulo ((monkey-op monkey-inst) item) mod)]
           [item-dest ((monkey-test monkey-inst) new-item-val)])
      (throw new-item-val item-dest)))
  (~>> (monkey-items monkey-inst)
       reverse
       (map produce-throw)))

(define (apply-throw throw-inst monkeys)
  (list-update monkeys
               (throw-to throw-inst)
               (lambda (monkey-inst) (monkey-add-item monkey-inst (throw-val throw-inst)))))

(define (distribute-items monkeys throws)
  (foldl apply-throw monkeys throws))

(define (monkey-round monkeys)
  (define mod
    (~>> monkeys
         (map monkey-div-by)
         (apply *)))
  (let loop ([monkeys monkeys]
             [index 0])
    (if (>= index (length monkeys))
        monkeys
        (loop
         (let* ([throws (take-turn (list-ref monkeys index) mod)]
                [cleared-active (list-update monkeys index monkey-clear-items)]
                [new-monkeys (distribute-items cleared-active throws)])
           new-monkeys)
         (add1 index)))))

(define (run-rounds monkeys round-cnt)
  (let loop ([monkeys monkeys]
             [round 0])
    (if (>= round round-cnt)
        monkeys
        (loop (monkey-round monkeys) (add1 round)))))

(define monkey-text
  (~>> raw-input
       (map string-trim)
       (filter (lambda (line) (non-empty-string? line)))
       (list-chunk _ 6)))

(define part1
  (~>> monkey-text
       (map (lambda (lines) (parse-monkey lines #t)))
       (run-rounds _ 20)
       (map monkey-inspect-cnt)
       (sort _ >)
       (take _ 2)
       (apply *)))

(define part2
  (~>> monkey-text
       (map (lambda (lines) (parse-monkey lines #f)))
       (run-rounds _ 10000)
       (map monkey-inspect-cnt)
       (sort _ >)
       (take _ 2)
       (apply *)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
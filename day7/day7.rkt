#lang racket

(require threading)
(require "../util/util.rkt")

(struct data-file (name size) #:transparent)
(struct directory (name objects size) #:transparent)

(define (object-size obj)
  (if (data-file? obj)
      (data-file-size obj)
      (directory-size obj)))

(define raw-input (file->lines "input.txt"))

(define (parse-ls-output commands)
  (let loop ([commands commands]
             [datafiles null])
    (if (or (empty? commands) (string-prefix? (first commands) "$"))
        (values commands datafiles)
        (match (first commands)
          [(regexp "^dir (.*)$" (list _ name))
           (loop (rest commands) datafiles)] ; ignores dirs
          [(regexp "^(.*) (.*)$" (list _ size name))
           (loop (rest commands) (cons (data-file name (string->number size)) datafiles))]))))

(define (parse-commands commands)
  (define (parse-commands-dir commands dir-name)
    (let loop ([commands commands]
               [datafiles null]
               [dir-objects null])
      (if (null? commands)
          (let* ([objects (append datafiles dir-objects)]
                 [size (apply + (map object-size objects))])
            (values null (directory dir-name objects size)))
          (match (first commands)
            [(regexp "^\\$ cd ..$")
             (let* ([objects (append datafiles dir-objects)]
                    [size (apply + (map object-size objects))])
               (values (rest commands) (directory dir-name objects size)))]
            [(regexp "^\\$ cd (.*)$" (list _ next-dir))
             (let-values ([(rest-commands new-directory) (parse-commands-dir (rest commands) next-dir)])
               (loop rest-commands datafiles (cons new-directory dir-objects)))]
            [(regexp "^\\$ ls$")
             (let-values ([(rest-commands new-datafiles) (parse-ls-output (rest commands))])
               (loop rest-commands (append new-datafiles datafiles) dir-objects))]))))
  
  (let-values ([(_ dir) (parse-commands-dir (rest commands) "/")]) dir))

(define (get-directory-size dir max-size)
  (let ([size (directory-size dir)])
    (if (< size max-size) size 0)))

(define (sum-dir-sizes dir max-size)
  (if (null? dir)
      0
      (~>> (directory-objects dir)
           (filter directory?)
           (map (lambda (inner-dir)
                  (+ (get-directory-size inner-dir max-size)
                     (sum-dir-sizes inner-dir max-size))))
           (apply +))))

(define total-disk-space 70000000)

(define (filter-directory-size dir min-size)
  (let ([size (directory-size dir)])
    (if (> size min-size) size total-disk-space)))

(define (find-smallest-dir-above dir min-size)
  (if (null? dir)
      total-disk-space
      (~>> (directory-objects dir)
           (filter directory?)
           (map (lambda (inner-dir)
                  (min (filter-directory-size inner-dir min-size)
                       (find-smallest-dir-above inner-dir min-size))))
           (cons total-disk-space)
           (apply min))))

(define final-dir (parse-commands raw-input))

(define part1
  (sum-dir-sizes final-dir 100000))
(define part2
  (let ([min-size (- 30000000 (- total-disk-space (directory-size final-dir)))])
  (find-smallest-dir-above final-dir min-size)))

(printf "part 1: ~a\n" part1)
(printf "part 2: ~a\n" part2)
#lang racket

(require "read-in.rkt")
(require "schedule-tester.rkt")

;; check some examples
(define catalog (preq-list-parser "preq-lists/all-cs-courses.txt"))
(define list-of-entries (hash-ref catalog 'ASTR3800))
(define all-possible-preq-lists (map course-preq list-of-entries))
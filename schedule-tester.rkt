#lang racket

(require "read-in.rkt")

;; functions to provide to other files
(provide passes-test?)
(provide is-valid?)
(provide goal-achieved?)

;; test a schedule for being valid and satisfying a goal list
(define (passes-test? schedule goal-list catalog)
  (and (is-valid? schedule catalog) (goal-achieved? schedule goal-list catalog)))

;; check if a schedule is valid
(define (is-valid? schedule catalog)
  ;; provide a base case
  (cond
    [(empty? schedule) #t]
    [else
     ;; reverseing the schedule makes it easier to work with
     (set! schedule (reverse schedule))
     (let [(courses-so-far (flatten (cdr schedule)))]
       (cond
         [(semester-valid? (car schedule) courses-so-far catalog)
           (is-valid? (reverse (cdr schedule)) catalog)]
         [else
          (display (reverse schedule))
          (display " contains courses whose prerequisits are not filled or semesters with a number of classes that is not between 4 and 6\n")
            #f]))]))

;; check goals achieved by a schedule
(define (goal-achieved? schedule goal-list catalog)
  (define taken-courses (flatten schedule))
  (andmap
   (lambda (goal-name)
     (satisfies-goal? taken-courses goal-name catalog))
   goal-list))

;; check that a goal is satisfied by a set of courses
(define (satisfies-goal? taken-courses goal-name catalog)
  (define goal-entry-list (hash-ref catalog goal-name))
  (define goal-type (course-type (car goal-entry-list)))
  (cond
    [(and (equal? goal-type 'COURSE) (not (member goal-name taken-courses))) #f]
    [else
     (ormap
      (lambda (course-entries)
        (andmap
         (lambda (single-course)
           (satisfies-goal? taken-courses single-course catalog))
         (course-preq course-entries)))
      goal-entry-list)]))

;; check that the semester is valid
(define (semester-valid? semester courses-so-far catalog)
  (cond
    ;; must have no more than 5 courses
    [(<= (length semester) 5)
     (andmap
      (lambda (course-name)
        ;; add the course itself to the courses so far
        ;; this is done because the satisfies-goal? function also checks that the course is taken
        (satisfies-goal? (cons course-name courses-so-far) course-name catalog))
      semester)]
    [else #f]))

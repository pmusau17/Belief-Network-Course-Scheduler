#lang racket

(provide (all-defined-out))

;; course structure which can be a course or an abstraction like a major
(define-struct course (preq name type) #:transparent)

;; structures for belief networds
(define-struct belief (prob grade course conditioned-on) #:transparent)
(define-struct condition-pair (course grade) #:transparent)

;; access belief network as two hash maps
;; one gives the beliefs for a variable
;; the other gives the variables updated by a change in beliefs of a variable
(define-struct belief-network (beliefs-map updates-map))

;; get the variables in the belief network
(define (get-variables network)
  (hash-keys (belief-network-beliefs-map network)))

;; turn a list of pairs into a list of conditions
(define (to-condition-pairs conditions)
  (map
   (lambda (condition)
     (cond
       [(list? condition)
        (condition-pair (car condition) (cadr condition))]
       [else (condition-pair (car condition) (cdr condition))]))
   conditions))

;; get the variables that condtion a variable's beliefs
(define (get-condition-names variable network)
  (map
   condition-pair-course
   (belief-conditioned-on (car (hash-ref (belief-network-beliefs-map network) variable)))))

;; get the variables that a variable conditions the beliefs of
(define (get-child-names variable network)
  (hash-ref (belief-network-updates-map network) variable))

;; get beliefs about a variable
(define (get-beliefs variable network conditions)
  (filter
   (lambda (belief)
     (andmap
      (lambda (condition)
               (cond
                 [(member variable (hash-ref (belief-network-updates-map network) (condition-pair-course condition)))
                  (member condition (belief-conditioned-on belief))]
                 [else #t]))
      conditions))
   (hash-ref (belief-network-beliefs-map network) variable)))

;; print beliefs about a variable
(define (print-beliefs variable network conditions)
  (map
   (lambda (belief)
     (display "P(")
     (display (belief-course belief))
     (display "=")
     (display (belief-grade belief))
     (display "|")
     (map
      (lambda (condition)
        (display (condition-pair-course condition))
        (display "=")
        (display (condition-pair-grade condition))
        (display ", "))
      (belief-conditioned-on belief))
     (display ") = ")
     (display (belief-prob belief))
     (display "\n"))
  (get-beliefs variable network conditions))
  null)

;; get the courses in a catalog
(define (get-courses catalog)
  (hash-keys catalog))

;; turn a belief list into a network
(define (make-network belief-list)
  (belief-network
   (make-hash
    (map
     (lambda (belief-group)
       (cons (belief-course (car belief-group)) belief-group))
     (group-by
      (lambda (belief)
        (belief-course belief))
      belief-list)))
  (get-edges belief-list)))

;; get the edes from variables to the things they effect
(define (get-edges belief-list)
  (make-hash
   (map
    (lambda (edge-group)
      (cons (car (car edge-group)) (map cdr edge-group)))
    (group-by
     (lambda (edge)
       (car edge))
     (remove-duplicates
      (foldr append null
             (map
              (lambda (belief)
                (map (lambda (condition)
                       (cons (condition-pair-course condition) (belief-course belief)))
                     (belief-conditioned-on belief)))
              belief-list)))))))

;; read in the preqs for each course and abstraction
(define (preq-list-parser filepath)
  (let ((lines (file->lines filepath #:mode 'text #:line-mode 'linefeed)))
    (catalog-list-to-map
     (map
      ;; turn the list of symbols into courses
      (lambda (l)
        (course (cddr l) (cadr l) (car l)))
      (map
       ;; turn the list into symbols instead of strings
       (lambda (l)
         (map string->symbol l))
       ;; split the line by spaces
       (map string-split lines))))))

;; turn each line into a belief
(define (interp-belief values)
  (let [
        (prob (string->number (car values)))
        (grade (string->number (cadr values)))
        (course (string->symbol (caddr values)))
        (conditions (cdddr values))
        ]
    (belief prob grade course
            (map (lambda (cond)
                   (let [(parts (string-split cond "="))]
                     (condition-pair (string->symbol (car parts)) (string->number (cadr parts)))))
                 conditions))))

;; catalog list to hash map where each entry is a list of possible preq sets
(define (catalog-list-to-map catalog-list)
  (make-hash
   (map
    (lambda (course-list)
      (cons
       (course-name (car course-list))
       course-list))
    (group-by
     (lambda (course-entry)
       (course-name course-entry))
     catalog-list))))

;; read in the beliefs for the belief network
(define (bayesian-network-parser filepath)
  (let ((lines (file->lines filepath #:mode 'text #:line-mode 'linefeed)))
    (make-network
     (map
      ;; turn the list into beliefs
      interp-belief
      ;; split the line by spaces
      (map string-split lines)))))
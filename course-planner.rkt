#lang racket

;-----------------------------------------------------------------------------------
;-----------------------------------------Author: Patrick Musau
;-----------------------------------------Group Members: Diego Manzanas, Tim Darrah
;Our Design:
;In this project we used depth first search to generate a list of classes that a 
;student needed to satisfy in order to fulfill the goals listed in goal conditions.
;Using this list we implemented a function to generate a schedule for the student by
;adding classes onto the schedule in the order that they satisfy their prerequisites.
;This can be seen in the generateSemesters helper function included in this document.
;In order to make the scheduler interactive during the first search we kept track of
;which courses had multiple ways of being satisfied and stored them in a hash table
;in order to have constant time access. As an example there are 43 ways to satisfy the 
;CSmathelective requirement. Thus the hashtable cointained a key 'CSmathelective that
;stored the current option chosen and the numberOf possible ways one could satisfy the
;CSmathelective. Thus, if the user wanted to generate a new schedule after the inital one
;we generated a new lidy of classes by randomly selecting a new way to satisfy each of
;the requirements for the courses and abstractions that had more than way of being satisfied.
;we made sure not to repeat schedules and calculated apriori how many possible schedules
;there were. So for instance there are 10,320 ways to satisfy CSmathematics but only one way
;to satisfy 'CS1101. Each time the user asked for a new combination we incremented
;the counter and selected a new combination. If there were no combinations to be chosen
;from then we returned false false. Thus the core of our algorithm makes use of DFS and
;a hash table for interactivity. This made returning schedules very efficient and thus
;once our algorithm begins it can return schedules immediately. We have noticed that loading
;catalog and parser requires about 512 to 1GB memory in racket and this takes some time
;however once everything is loaded our algorithm works really fast.
;--------------------------------------------------------------------------------------

(require "read-in.rkt")
(require "schedule-tester.rkt")
;for hash tables 
(require data/heap)
;; for queues
(require data/queue)

;----------------------------------------------------------------------------------
;---------------------------------------------------------- STACK / QUEUE FUNCTIONS
;----------------------------------------------------------------------------------

;; some functions for stacks, sets and queues
;; stacks
(define (make-stack)
  (list))

(define (push-stack x st)
      (cons x st))

(define (pop-stack st)
  (values (car st) (cdr st)))

(define (stack-empty? st)
  (empty? st))

;; queues
;; to make a new queue, use the make-queue function
(provide make-queue)

(define (push-queue x queue)
      (enqueue! queue x)
  queue)

(define (pop-queue queue)
  (let ([output (dequeue! queue)])
    (values output queue)))

(provide queue-empty?)

;; some more functions for viewing queues
(define (length-queue queue)
  (length (queue->list queue)))

(provide queue->list)

(define (ismember? elm lst)
  (ormap [lambda (val) (equal? val elm)] lst))

(define (listempty? ls)
  (null? ls))

(define (is-class? course)
  (equal? (course-type course) 'COURSE))

(define (printCourses ls)
  (map (lambda (i) (println (course-name i)))ls)
  )

(define (add-end-stack x st)
     (flatten (cons st x)))


;function that prints out a list
(define (printList ls)
  (map (lambda(i) (println i)) ls
  )
  )

;*********************************************************************************
;                      some helper functions are listed here
;*********************************************************************************


;----------------------------------------------------------------------------------
;------------------------------------------------------------------------------MAX
;----------------------------------------------------------------------------------
;function to get max of a list of integers
(define (max ls)
  (define maxnum -10)
  (map (lambda (i)
         (cond [(> i maxnum) (set! maxnum i)]
               [else (set! max max)])) ls)
  maxnum
  )

;----------------------------------------------------------------------------------
;----------------------------------------------------------------------COURSE COUNT
;----------------------------------------------------------------------------------
;; function to find out how many courses there are in a semester
(define (count-courses semester)
  (define count 0)
  (define coursetype null)
  (map (lambda (i)
         (set! coursetype (course-type i))
         (cond [(equal? coursetype 'COURSE) (set! count (+ count 1))][else (set! count count)])
         ) semester)
  count
  )

;----------------------------------------------------------------------------------
;------------------------------------------------------------------------DELETE NTH
;----------------------------------------------------------------------------------
;funtion to delete an item at a specific index within a list
;-----------------------------------------------------------
(define (deleteNth n l)
  (cond
    [(= n 0) (rest l)]
    [(< n (length l)) (append (take l n) (rest (drop l n)))]
    [else l])
  #;
  (cond
    [(empty? l) l]
    [(zero? n) (rest l)]
    [else (cons (first l) (deleteNth (sub1 n) (rest l)))])
  #;
  (cond
    [(and (zero? n) (empty? l)) l]
    [(and (positive? n) (empty? l)) l]
    [(and (zero? n) (cons? l)) (rest l)]
    [(and (positive? n) (cons? l)) (cons (first l) (deleteNth (sub1 n) (rest l)))]))


;----------------------------------------------------------------------------------
;-------------------------------------------------------------------------INSERT AT
;----------------------------------------------------------------------------------
;function to add an item at a specific index within a list
;----------------------------------------------------------
(define (insert-at lst pos x)
  (define-values (before after) (split-at lst pos))
  (append before (cons x after)))


;*********************************************************************************
;       the following functions are for generating different semester schedules
;       a hash table is used to manage the semester combinations
;*********************************************************************************

;----------------------------------------------------------------------------------
;------------------------------------------------------------------GET COURSE INDEX
;----------------------------------------------------------------------------------
;function to select the correct course index
(define (getCourseIndex courseName ht)
  (define courseTuple (getOptionList courseName ht))
  (list-ref courseTuple 0)
  )

;----------------------------------------------------------------------------------
;-----------------------------------------------------------------UPDATE HASH TABLE
;----------------------------------------------------------------------------------
;function that updates the existing hashtable if it exists
(define (updateHashTable ht combination)
(define currentTuple null)
(define currentIndex 0)
(define newTupleVal null)
(define newTuple null)
(define keys (hash-keys ht))
 (map (lambda (i)
        (set! currentTuple (hash-ref ht i))
        (set! newTupleVal (list-ref combination currentIndex))
        (set! newTuple (list newTupleVal (list-ref currentTuple 1)))
        ;(println newTupleVal)
        (hash-set*! ht i newTuple)
        ;(println i)
        ;(set! tupleList (insert-at tupleList currentIndex (list-ref currentTuple 0)))
        (set! currentIndex (+ currentIndex 1))
        )
  keys)
  ;tupleList
  )


;----------------------------------------------------------------------------------
;---------------------------------------------------------------GENERATE HASH TABLE
;----------------------------------------------------------------------------------
;function that creates a hash table for the options
(define (generate-hash-table courseName catalog hashtable)
  ;create a hash table
  ;(define ht (make-hash))
  (define ht hashtable)
  ;create key
  (define key (~a courseName))
  ;get all of the options for the course
  (define options (hash-ref catalog courseName))
  ;find the length of this list
  (define numberOfOptions (length options))
  ;define a tuple where the first item is the current index and the second item is the upper bound
  (define tuple (list 0 numberOfOptions))
  (hash-set! ht key tuple)
  ht)

;----------------------------------------------------------------------------------
;-------------------------------------------------------------------GET OPTION LIST
;----------------------------------------------------------------------------------
;function that gets the list of upper and lower bounds of the number of options for a course
(define (getOptionList courseName ht)
  (define elem null)
  (set! elem (hash-ref ht (~a courseName)))
  elem
  )

;----------------------------------------------------------------------------------
;---------------------------------------------------------------COURSE HAS OPTIONS?
;----------------------------------------------------------------------------------
;function that checks if a course has options and if it does adds its bounds to the hashtable
(define (courseHasOptions? courseName ht catalog)
  (define hashTable ht)
  (define options (hash-ref catalog courseName))
  (define numOptions (length options))
  ;boolean to indicate that a course has options
  (cond [(> numOptions 1)
         (set! hashTable (generate-hash-table courseName catalog hashTable))
         ]
        [else (set! hashTable hashTable)
         ])
  hashTable
  )


;----------------------------------------------------------------------------------
;----------------------------------------------------------GENERATE NEW COMBINATION
;----------------------------------------------------------------------------------
;function that generates a new random combination given a list of the previous combinations and iteration number
(define (generateNewCombination ht previousCombination)
 (define tupleList null)
 (define currentTuple null)
 (define currentIndex 0)
 (define upperBound 0)
;define variable that says whether this combination is unique
(define unique #f)
;this is for later (hash-has-key? hash key)
(define keys (hash-keys ht))
  ;make sure you generate a new combination
 (for ([i (in-naturals 0)]
      #:break unique)
    (map (lambda (i)
        ;(println i)
        (set! currentTuple (hash-ref ht i))
        (set! upperBound (list-ref currentTuple 1))
        (set! tupleList (insert-at tupleList currentIndex (random 0 upperBound)))
        (set! currentIndex (+ currentIndex 1))
        )
  keys)
  ;check if this combination has already been used
  (set! unique ( not (ismember? tupleList previousCombination)))
  ;(println unique)
  )
  tupleList
)

;----------------------------------------------------------------------------------
;--------------------------------------------------------------GENERATE COMBINATION
;----------------------------------------------------------------------------------
;helper function for generate new combination
;function that generates a tuple containing the current combination being tried
(define (generateCombination ht)
 (define tupleList null)
 (define currentTuple null)
 (define currentIndex 0)
 (define keys (hash-keys ht))
 (map (lambda (i)
        (set! currentTuple (hash-ref ht i))
        ;(println i)
        (set! tupleList (insert-at tupleList currentIndex (list-ref currentTuple 0)))
        (set! currentIndex (+ currentIndex 1))
        )
  keys)
  tupleList
  )

;----------------------------------------------------------------------------------
;--------------------------------------------------------CALCULATE NUM COMBINATIONS
;----------------------------------------------------------------------------------
;function that calculates the number of combinations available
(define (calculateNumCombinations ht)
 (define numbCombinations 1)
 (define currentTuple null)
 (define currentIndex 0)
;this is for later (hash-has-key? hash key)
 (define keys (hash-keys ht))
 (map (lambda (i)
        (set! currentTuple (hash-ref ht i))
        (set! numbCombinations (* numbCombinations (list-ref currentTuple 1)))
        (set! currentIndex (+ currentIndex 1))
        )
  keys)
   numbCombinations
  )


;----------------------------------------------------------------------------------
;----------------------------------------------------------------GENERATE SEMESTERS
;----------------------------------------------------------------------------------
;creates a list of semesters (schedule) from a list of classes
(define (generateSemesters classList)
  ;;intialize the classStack to be all of the classes that we need to insert into the 
  (define classStack classList)
  ;;define the schedule to be an empty list of lists
  (define semester (list))
  (define semesterNames (list))
  ;;define currentClass, insertionability, insertionIndex
  (define currentClass null)
  (define canBeInserted null)
  (define insertionIndex null)
  ;;loop through all the classes that need to be added 
  (for ([i (in-naturals 0)]
      #:break (stack-empty? classStack))

    ;pop the class from the top of the stack
    (set!-values (currentClass  classStack) (pop-stack  classStack))
    (set!-values (canBeInserted insertionIndex) (whereToInsert? currentClass semesterNames))
    ;check if the class can be added to a specific semester if not put it back onto the stack at the end
    ;(fprintf (current-output-port) "Current Class: ~a | Can be Inserted? : ~a | insertionIndex: ~a.\n" (course-name currentClass) canBeInserted insertionIndex)

    (cond [canBeInserted
           (set!-values (semesterNames semester) (insertClass currentClass insertionIndex semesterNames semester))

           ]
          [else ;(println "add this back on to the end of the stack")
                 (set! classStack (add-end-stack currentClass classStack))
                ]
          )
    
    )
  (values semesterNames semester)
  )


;----------------------------------------------------------------------------------
;----------------------------------------------------------------------INSERT CLASS
;----------------------------------------------------------------------------------
;function that inserts a class at a specific index
(define (insertClass class index semesterListNames semesterList)
  ;define values for the semesterListNames and semesterList
  (define semesterNames semesterListNames)
  (define semesterLs semesterList)
  (define innerSemester null)
  (define innerSemesterlen null)
  (define innerSemesterNames null)
  (define currIndex index)
   "ayyee hol up im working on this"
  ;find out how many semesters there are
  (define numberOfSemesters (length semesterListNames))
  ;;if we need to add it an index that is larger than the largest index we need to generate a new semester
  (cond [(>= index numberOfSemesters)
         (define newSemester (make-stack))
         (define newSemesterNames (make-stack))
         ;push the class into this new semester and newSemesterNames
         (set! newSemester (push-stack class newSemester))
         (set! newSemesterNames (push-stack (course-name class) newSemesterNames))
         ;add this semester onto the end of the semesters
         (set! semesterNames (insert-at semesterNames index newSemesterNames))
         (set! semesterLs (insert-at semesterLs index newSemester))
         ]
        ;otherwise add it where it is supposed to be if the number of classes within that semester is less than 5
         [else
          (set! innerSemester (list-ref semesterList index))
          (set! innerSemesterNames (list-ref semesterListNames index))
          (set! innerSemesterlen (length innerSemester))
          (cond[(< innerSemesterlen 5)

                ;add the item to the end of the current semesters
                (set! innerSemester (add-end-stack class innerSemester))
                (set! innerSemesterNames (add-end-stack (course-name class) innerSemesterNames))
                ;update the semester lists correctly by first deleting the old semesters
                (set! semesterNames (deleteNth index semesterNames))
                (set! semesterLs (deleteNth index semesterLs))
                ;add in the correct semesters now
                (set! semesterNames (insert-at semesterNames index innerSemesterNames))
                (set! semesterLs (insert-at semesterLs index innerSemester))
                ]
               [else ;(println "Got here")
                     ;if it can't be inserted here 
                     (set! currIndex (+ currIndex 1))
                     (set!-values (semesterNames semesterLs) (insertClass class currIndex semesterNames semesterLs))
                     ])
          ])
  (values semesterNames semesterLs)
  )

;----------------------------------------------------------------------------------
;-------------------------------------------------------------------WHERE TO INSERT
;----------------------------------------------------------------------------------
;function that determines if and where a node can be inserted into the list
(define (whereToInsert? class semesterListNames) ;need to add a parameter for semesterCourseNames

  ;define a boolean specifying if the class can be added
  (define canbescheduled #f)
  ;define the course prerequisites
  (define preqs (course-preq class))
  ;define holder values for the return types of isScheduled?
  (define scheduleBoolean null)
  (define semesterIndex null)
  ;define a list that will be the value if all prerequisites have been satisfied
  (define allSatisfied (make-stack))
  ;define a list that indicates which semester the courses have been satisfied in
  (define semestersSatisfied (make-stack))
  ;a class can be scheduled if all of its prereqs have been added
  (cond
    ;if a class has no prereqs it can be scheduled so we will schedule it at the earliest semester it can be added
    [(equal? (length preqs) 0) (set! canbescheduled #t) (set! allSatisfied (push-stack #t allSatisfied)) (set! semestersSatisfied (push-stack -1 semestersSatisfied))]
    ;otherwise we need to check if all of its pre-reqs have been scheduled
    [else (map (lambda (i)
            ;check if the current prereq has been satisfied
            (set!-values (scheduleBoolean semesterIndex) (isScheduled? i semesterListNames))
            ;add the boolean to all satisfied
            (set! allSatisfied (push-stack scheduleBoolean allSatisfied))
            ;add all the indexes onto the stack that stores semesters satisfied
            (set! semestersSatisfied (push-stack semesterIndex semestersSatisfied))
            )
          preqs)
            ;in order for you to add it to the list all of the prereqs must be satisfied.
            ;thus #f should never appear in allSatisfied
            (set! canbescheduled (not (ismember? #f allSatisfied)))
          ]
    )
  (cond [canbescheduled (set! semesterIndex (+ (max semestersSatisfied) 1))]
        [else (set! semesterIndex -1)])
  (values canbescheduled semesterIndex))

;----------------------------------------------------------------------------------
;----------------------------------------------------------------------IS SCHEDULED
;----------------------------------------------------------------------------------
;function to see if a class is on the semester list somewhere
(define (isScheduled? className semesterList)
  (define scheduled #f)
  (define semesterindex -1)
  (define index 0)
  (cond[(equal? (length semesterList) 0) (set! scheduled #f)]
       [else (map (lambda (i)
                    (define member (ismember? className i))
                    (cond [member (set! scheduled #t) (set! semesterindex index) (set! index (+ index 1))]
                          [else (set! scheduled scheduled) (set! index (+ index 1))]))
                  semesterList)])
  (values scheduled semesterindex)
  )



;***********************************************************************************
;     helper functions for calculating the grades and working with the bayes
;     net are listed here
;***********************************************************************************

;----------------------------------------------------------------------------------
;----------------------------------------------------------------GET EXPECTED GRADE
;----------------------------------------------------------------------------------
;-----------------------------------------------------------------------
;get the expected grade of a class given a list of beliefs
;-----------------------------------------------------------------------
(define (get-expected-grade class list-of-beliefs)
  (define prob 0)
  (define expected-grade 0.0)
  ;;go thru the list of beliefs and calculate the expected grade
  (for ((i (length list-of-beliefs)))
      (set! prob (belief-prob (list-ref list-of-beliefs i)))
      (set! expected-grade (+ expected-grade (* i prob)))
  )
;;need to round the grade to a whole number before returning
(exact-round expected-grade))


;----------------------------------------------------------------------------------
;--------------------------------------------------------------------MAKE CONDITION
;----------------------------------------------------------------------------------
;converts a list of prereqs and grades to a list of pairs
;-----------------------------------------------------------------------
(define (make-condition condition-on these-grades)
      (define temp '())
      (define pair '())
      (cond
          ;;ensure the length of conditions (prereqs) and grades are equal
          ;;they always should be.
          [(eq? (length condition-on) (length these-grades))
              (for ((i (length condition-on)))
                  ;;create a pair (<course> <grade>)
                  (set! pair (list (list-ref condition-on i) (list-ref these-grades i)))
                  ;;add the pair to the list
                  (set! temp (cons pair temp)))
              ;;since cons adds to the front, we need to reverse the list
              ;;and then create a conditioned pair of it
              (set! temp (reverse temp)) (to-condition-pairs temp)]
          [else (print "not equal.") #f]))


;----------------------------------------------------------------------------------
;-------------------------------------------------------------------PREREQS GRADED?
;----------------------------------------------------------------------------------
;checks if a courses prerequesites have been taken
;-------------------------------------------------
(define (prereqs-graded? graded-courses current-course course-prereqs)
    ;;number of prereq courses needed
    (define needed-count (length course-prereqs))
    ;;count to track the the number of prereqs encountered 
    (define current-count 0)
    ;;list of grades of the prereqs
    (define grades '())
    ;;go thru the prereqs and subsequentely the graded courses
    (for ((p course-prereqs))
        (for ((c graded-courses)
             ;;if we found all the prereqs stop searching
             #:break (eq? current-count needed-count))
            (cond
                ;;if we found a prereq add it to the grades
                [(equal? p (list-ref c 0))
                    (set! current-count (+ 1 current-count))
                    ;;again some list manipulation to get the right form
                    (set! grades (flatten (cons grades (list-ref c 1))))]
                [else
                    (set! current-count current-count)]
            )
        )
    )
    (cond
        ;;return the list of grades if they were all found
        [(eq? current-count needed-count) grades]
        ;;otherwise return false
        [else #f]
    )
)

;***********************************************************************************
;                        main deliverables follow
;***********************************************************************************

;----------------------------------------------------------------------------------
;------------------------------------------------------------------GET EXPECTED GPA
;----------------------------------------------------------------------------------
;get the GPA of the expected schedule and list of current class grades
;-----------------------------------------------------------------------
(define (get-expected-gpa schedule network grades-for-taken-classes)
  ;;get the list of grades
  (define grades (get-expected-values schedule network grades-for-taken-classes))
  ;;need the total number of classes for the average formula
  (define number-classes (length grades))
  ;;each course grade gets added to this value
  (define total-grades 0)
  ;;the current grade
  (define current-grade 0)
  ;;overall gpa
  (define gpa 0.0)
  ;;loop thru the grades and add them all up
  (for ((val grades))
      (set! current-grade (list-ref val 1))
      (set! total-grades (+ total-grades current-grade))
  )
  ;;divide the total grades by number of classes
  (set! gpa (/ total-grades number-classes))
  ;;return the gpa for the schedule
 gpa)

;----------------------------------------------------------------------------------
;---------------------------------------------------------------GET EXPECTED VALUES
;----------------------------------------------------------------------------------
;-----------------------------------------------------------------------
;gets the expected grades for each class in a schedule
;-----------------------------------------------------------------------
(define (get-expected-values schedule network grades-for-taken-classes)
    ;;number of semesters in the schedule
    (define semester-count 0)
    ;;the current class is conditioned on...
    (define conditioned-on null)
    ;;this condition is a conditioned-pairs list
    (define this-condition null)
    ;;list of beliefs for a class given its conditions
    (define this-belief null)
    ;;the grade for a class
    (define this-grade 0)
    ;;the grades for a classes prereqs
    (define preq-grades null)
    ;;loop through each semester in the schedule
    (for ((semester schedule))
        ;;loop thru each course in the semester
        (for ((this-course semester))
            ;;get the prereqs of this-course
            (set! conditioned-on (get-condition-names this-course network))
            (cond
                ;;if its the first semester, just get the grades
                [(= 0 semester-count)
                   ;;just get the belief, there are no conditioned pairs
                   (set! this-belief (get-beliefs this-course network '()))]
                [else
                   ;;get the prereq grades if it is not the first semester
                   (set! preq-grades (prereqs-graded? course-grades this-course conditioned-on))
                   (cond
                       ;;should always evaluate to true, otherwise the schedule is bogus
                       [(list? preq-grades)
                             ;;get the conditioned-pairs list 
                             (set! this-condition (make-condition conditioned-on preq-grades))
                             ;;get the belief for the class with its conditions
                             (set! this-belief (get-beliefs this-course network this-condition))]
                       [else (println "not taken!!")] ;<---- SHOULD NEVER GET HERE!!
                    )]
            )
            ;;assign the grade with a helper function
            (set! this-grade (get-expected-grade this-course this-belief))
            ;;add to the list
            (set! course-grades (cons (list this-course this-grade) course-grades))
            ;(print "course grades: ") (println (list this-course this-grade))
        )
        ;;increment the semester count
        (set! semester-count (+ 1 semester-count))
    )
  ;;return the list of grades
  course-grades)


;----------------------------------------------------------------------------------
;------------------------------------------------------------------SCHEDULE ADVISOR
;----------------------------------------------------------------------------------
;function that does all of the functionality for the scheduler
;-------------------------------------------------------------
(define (schedule-advisor catalog goals network interacting)
  ;;define the goal stack
  (define goalStack (make-stack))
  (set! goalStack goals)
  ;;define a hashtable that tracks which courses have options
  (define ht (make-hash))
  ;;create a list for visitedNodes
  (define visitedNodes (make-stack))
  ;;create a list of all the courses I need
  (define neededClasses (make-stack))
  ;define a var for current, options, currentCourse, preReqs, courseType, scheduleCoursesNames, scheduleCourses
  (define current null)
  (define options null)
  (define currentCourse null)
  (define prereqs null)
  (define courseType null)
  (define courseHasOpts null)
  (define newCombo null)
  (define courseIndex null)
  (define scheduleCoursesNames null)
  (define scheduleCourses null)
  (define schedule-gpa 0.0)
  ;define variables for the initial run and a list for the previous combinations tried
  (define initRun? #t)
  (define previousCombos (list))
  ;define whether the search should be interactive
  (define continueSearching #t)
  ;define a variable for the numberOfCombinations
  (define numCombinations 0)
  ;define a variable to track what number search you are on so that we can terminate if there aren't any others
  (define iterationNumber 1)


  ;interactionLoop
  (for ([i (in-naturals 0)]
      #:break (equal?  continueSearching #f))
    ;reset the neededClasses and visitedNodes for following interations
    (set! neededClasses (make-stack))
    (set! visitedNodes (make-stack))
  ;searchloop
    (for ([i (in-naturals 0)]
      #:break (stack-empty? goalStack)) ;while the goal stack is not empty
      ;;get the current at the top of the stack
      (set!-values (current goalStack) (pop-stack goalStack))
      ;;add it to the list of visitedNodes
      (set! visitedNodes (push-stack current visitedNodes))
      ;;get the options associated with that node
      (set! options (hash-ref catalog current))
      ;if its the intial run only take the option and add its name to the hashtable to the index
      ;otherwise if it has options get the correct index from the hashtable if its in there
      (cond [initRun? ;is this the first run check if it has options and add it to the hashtable
             (set! ht (courseHasOptions? current ht catalog))
             (set! currentCourse (list-ref options 0))
             ]
            [else
              ;check if the course has options by checking for its name in the hashtable
              (set! courseHasOpts (hash-has-key? ht (~a current)))
              ;if it indeed has options get the correct index from the hashtable
              (cond [courseHasOpts
                   ;get the correct courseIndex
                   (set! courseIndex (getCourseIndex current ht))
                   ;get the course at that index
                   (set! currentCourse (list-ref options courseIndex))
              ]
              ;if the course doesn't have any options only get the first one
                  [else
                   (set! currentCourse (list-ref options 0))
                   ])
           ])
      ;get the prerequisites for the course
      (set! prereqs (course-preq currentCourse))
      ;get the type of the course
      (set! courseType (course-type currentCourse))
      ;;if its a course add it to the classes we need to take list if its not there
      (cond [(and (not (ismember? currentCourse neededClasses)) (is-class? currentCourse))
             (set! neededClasses (push-stack currentCourse neededClasses))])    
      ;add all the prerequsites that have not been visited to the stack
      (map (lambda (i)
             (cond [(ismember? i visitedNodes) (set! goalStack goalStack)]
                   [else  (set! goalStack (push-stack i goalStack))])
             ) prereqs)
    )
 
    ;if this is the initial run then add the initial combo to the previous combos and generate a new one  
    (cond [initRun?
           ;add the intial combination to the hashtable which in this case is all zeros
           (set! previousCombos (push-stack (generateCombination ht) previousCombos))
           ;calculate how many combinations there are so we know when to halt
           (set! numCombinations (calculateNumCombinations ht))
           ;(println numCombinations)
           (fprintf (current-output-port) "Schedule: ~a out of ~a Possible Schedules.\n" iterationNumber numCombinations)
           (println "--------------------------")
           ;if there are more than one combination then we want to generate a new one
           (cond [(< iterationNumber numCombinations)
                  (set! newCombo (generateNewCombination ht previousCombos))
                  (set! previousCombos (push-stack newCombo previousCombos))
                  ;(println previousCombos)  
                  (updateHashTable ht newCombo)
                  ;(println ht)
                  ;put the goal conditions onto the goal stack so that we can search again
                  (set! goalStack goals)
                  ;increment the iteration counter
                  (set! iterationNumber (+ iterationNumber 1))
                  ]
                 [else
                    ;if there aren't any more combinations we want to stop searching and return false false
                    (set! continueSearching #f)
                  ])
           (println "Your Schedule is:")
           (set!-values (scheduleCoursesNames scheduleCourses) (generateSemesters neededClasses))
           ;get the GPA for the semester
           (set! schedule-gpa (get-expected-gpa scheduleCoursesNames network '()))
           ;set initRunToFalse
           (set! initRun? #f)
           ] ;if its not the initial generate a new combination and update the hash table
           [else
             (fprintf (current-output-port) "Schedule: ~a out of ~a Possible Schedules.\n" iterationNumber numCombinations)
             (println "-------------------------------------------------------")
             ;if there are more than one combination then we want to generate a new one
             (cond [(< iterationNumber numCombinations)
                (set! newCombo (generateNewCombination ht previousCombos))
                (set! previousCombos (push-stack newCombo previousCombos))
                ;(println previousCombos)  
                (updateHashTable ht newCombo)
                ;(println ht)
                ;put the goal conditions onto the goal stack so that we can search again
                (set! goalStack goals)
                ;increment the iteration counter
                (set! iterationNumber (+ iterationNumber 1))
                (println "Your Schedule is:")
                (set!-values (scheduleCoursesNames scheduleCourses) (generateSemesters neededClasses))
                ;get the GPA for the semester
                (set! schedule-gpa (get-expected-gpa scheduleCoursesNames network '()))
                ]
               [else
                ;if there aren't any more combinations we want to stop searching and return false false if they try again
                (set! continueSearching #f)
                ;still want to generate a schedule in this case
                (set!-values (scheduleCoursesNames scheduleCourses) (generateSemesters neededClasses))
                ;get the GPA for the semester
                (set! schedule-gpa (get-expected-gpa scheduleCoursesNames network '()))
                ])
         ])
  (println scheduleCoursesNames)
  (println "-------------------------------------------------------")  
  (fprintf (current-output-port) "Expected GPA for schedule: ~a\n" (real->decimal-string schedule-gpa 2))
  (println "-------------------------------------------------------")  
  (fprintf (current-output-port) "Total Number of Classes: ~a\n" (length neededClasses))
  (println "-------------------------------------------------------")  
  ;interacting question  
  (cond [interacting
         (println "Want to generate another schedule? (Y or N)")
         (define input (read))
         (cond [(or (equal? input 'N) (equal? input 'n)) (set! interacting #f) (set! continueSearching #t)]
             [(or (equal? input 'Y) (equal? input 'y)) (set! interacting #t)]
             [else 
              (println "Invalid Input: please enter either Y or N")
              (println "exiting scheduler")
              (set! interacting #f)
              (set! continueSearching #t)])
         ;if continueSearching is already false we need to change need clases to #f
         (cond[(equal? continueSearching #f)
               (set! scheduleCoursesNames #f)
               (set! schedule-gpa #f)
               (println "Sorry there aren't any more possible schedules to generate")
               ]
              [else
               (set! continueSearching interacting)
               ])
         ]
        [else
         ;if we aren't interacting stop the search
         (set! continueSearching interacting)])  
  )
  ;return correctly
  (cond [(equal? schedule-gpa #f)
         (values scheduleCoursesNames schedule-gpa)
         ]
        [else
         (values scheduleCoursesNames (string->number (real->decimal-string schedule-gpa)))
         ])
  )


;-----------------------------------------------------------------------
;---------------------------------------------------------PSEUDO MAIN
;-----------------------------------------------------------------------
(define course-grades '())
;;All of the tests we ran
;(define goalconditions (list 'CSmathematics 'CS4959 'CS1151))
(define goalconditions (list 'CSmajor))
;(define goalconditions (list 'CS4959 'CS1151))
;(define goalconditions (list 'CS1101))
;(define goalconditions (list 'SPAN4335))
;(define goalconditions (list 'MATH2410))
(define catalog (preq-list-parser "preq-lists/all-cs-courses.txt"))
(define network (bayesian-network-parser "bayesian-networks/all-cs-courses.txt"))
(define-values (courses gpa) (schedule-advisor catalog goalconditions network #t))
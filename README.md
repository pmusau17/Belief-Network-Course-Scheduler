# Belief-Network-Course-Scheduler

This project involved writing code that plans course schedules and estimates the expected GPAs for each schedule using a belief network. The belief network is also be used to revise GPA estimates for a student using a schedule given grades in certain courses found in schedule.

Our task was to write the top-level function called schedule-advisor, which is an interactive algorithm that finds
two values: a schedule and the expected GPA of that schedule prior to any courses being taken.
After returning and listing the first (schedule, GPA) pair, the user is queried as to whether they
want to see another (different) schedule for the same goal conditions, and its estimated a priori
GPA; if ‘yes’ then the next (schedule, GPA) pair is listed for the original goals, followed by the
same query of whether to continue or not; this can continue indefinitely

The main file in this repositiory is course-planner.rkt. The course catalogs used to generate schedules are stored in the directory ``preq-lists``

**To run the code run**: 

To load the course catalog: 

``(define catalog (preq-list-parser "preq-lists/all-cs-courses.txt"))``

To load the bayersian network: 

``(define network (bayesian-network-parser "bayesian-networks/all-cs-courses.txt"))``

To load a student's goal conditions: 

``(define goalconditions (list 'CSmajor))``

Other Examples: 
1. ``(define goalconditions (list 'CS4959 'CS1151))``
2. ``(define goalconditions (list 'CS1101))``
3. ``(define goalconditions (list 'SPAN4335))``
4. ``(define goalconditions (list 'MATH2410))``
5. ``(define goalconditions (list 'CSmathematics))``
6. ``(define goalconditions (list 'CSwritingrequirement))``
See the file "preq-lists/all-cs-courses.txt" for more options

To generate the schedule:

``(schedule-advisor catalog goal-conditions network interact)``

For example: 

``(define-values (courses gpa) (schedule-advisor catalog goalconditions network #t))``

It returns a schedule and an expected GPA

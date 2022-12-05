(define-module (day-5 supply-stacks)
  #:use-module (test)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define test-input "
    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
")

(define (tests)
  (in-test-suite ("Day 5: Supply Stacks")
    (test-equal "CMZ" (star-1 test-input))))





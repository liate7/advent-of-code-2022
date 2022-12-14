(define-module (day-13 distress-signal)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-input
  '("[1,1,3,1,1]"
    "[1,1,5,1,1]"
    ""
    "[[1],[2,3,4]]"
    "[[1],4]"
    ""
    "[9]"
    "[[8,7,6]]"
    ""
    "[[4,4],4,4]"
    "[[4,4],4,4,4]"
    ""
    "[7,7,7,7]"
    "[7,7,7]"
    ""
    "[]"
    "[3]"
    ""
    "[[[]]]"
    "[[]]"
    ""
    "[1,[2,[3,[4,[5,6,7]]]],8,9]"
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"
    ""))

(define (tests)
  (in-test-suite ("Day 13: Distress Signal")
    (test-equal (star-1 test-input) 13)))



(define (star-1 lines)
  (error "Not implemented yet"))

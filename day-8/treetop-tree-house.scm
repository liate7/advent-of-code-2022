(define-module (day-8 treetop-tree-house)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-input
  '("30373"
    "25512"
    "65332"
    "33549"
    "35390"
    ""))

(define (tests)
  (in-test-suite ("Day 8: Treetop Tree House")
    (test-equal (star-1 test-input) 21)))





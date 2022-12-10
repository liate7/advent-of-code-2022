(define-module (day-9 rope-bridge)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-input
  '("R 4"
    "U 4"
    "L 3"
    "D 1"
    "R 4"
    "D 1"
    "L 5"
    "R 2"
    ""))

(define (tests)
  (in-test-suite ("Day 9: Rope Bridge")
    (test-equal (star-1 test-input 13))))




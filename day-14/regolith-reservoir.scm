(define-module (day-14 regolith-reservoir)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-input
  '("498,4 -> 498,6 -> 496,6"
    "503,4 -> 502,4 -> 502,9 -> 494,9"
    ""))

(define (tests)
  (in-test-suite ("Day 14: Regolith Reservoir")
    (test-equal (star-1 test-input) 24)))



(define (star-1 lines)
  (error "Not implemented yet"))

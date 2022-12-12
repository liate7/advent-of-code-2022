(define-module (day-12 hill-climbing)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 exceptions)
  #:use-module (srfi srfi-1)
  #:use-module (pipe)
  #:use-module (fibers))

(define test-input
  '("Sabqponm"
    "abcryxxl"
    "accszExk"
    "acctuvwj"
    "abdefghi"
    ""))

(define (tests)
  (in-test-suite ("Day 12: Hill Climbing Algorithm")
    (test-equal (star-1 test-input) 31)))



(define (star-1 lines)
  (error "Not yet implemented"))

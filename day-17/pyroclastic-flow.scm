(define-module (day-17 pyroclastic-flow)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))

(define rocks
  (circular-list
   "####"
   (format #f ".#.~%~
               ###~%~
               .#.")
   (format #f "..#~%~
               ..#~%~
               ###")
   (format #f "#~%~
               #~%~
               #~%~
               #")
   (format #f "##~%~
               ##")))

(define test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(define (tests)
  (in-test-suite ("Day 17: Pyroclastic Flow")
    (test-equal (star-1 test-input) 3068)))



(define (star-1 lines)
  (error "Not implemented yet"))

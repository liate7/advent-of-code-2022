(define-module (day-17 pyroclastic-flow)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu))

(define rocks
  (map (curry list->array 2)
       '(((#\# #\# #\# #\#))
         ((_   #\# _ )
          (#\# #\# #\#)
          (_   #\# _ ))
         ((_   _   #\#)
          (_   _   #\#)
          (#\# #\# #\#))
         ((#\#)
          (#\#)
          (#\#)
          (#\#))
         ((#\# #\#)
          (#\# #\#)))))

(define test-input ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>")

(define (tests)
  (in-test-suite ("Day 17: Pyroclastic Flow")
    (test-equal (star-1 test-input) 3068)))



(define-immutable-record-type <chamber>
  (%make-chamber repr rocks jets)
  chamber?
  (repr  chamber-list  set-chamber-list)
  (rocks chamber-rocks set-chamber-rocks)
  (jets  chamber-jets  set-chamber-jets))

(define char->direction 
  (match-lambda
    (#\> 'right)
    (#\< 'left)))

(define (parse-directions input)
  (map char->direction (string->list input)))

(define (make-chamber input)
  (%make-chamber '()
                 (apply circular-list rocks)
                 (apply circular-list
                        (parse-directions input))))

(define-immutable-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x)
  (y point-y set-point-y))

(define (display-point point port)
  (format port "#<point ~a ~a>"
          (point-x point)
          (point-y point)))

(set-record-type-printer! <point> display-point)

(define (piece-step chamber left-edge-position)
  ())

(define (star-1 input)
  (error "Not implemented yet"))

(define-module (advent-of-code-2022 utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define-public (flatten lst) (list-transduce tflatten rcons lst))

(define-public (get-all-lines port)
  (string-split
   (get-string-all port)
   #\newline))

(define-public (sum lst)
  (apply + lst))

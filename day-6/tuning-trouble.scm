(define-module (day-6 tuning-trouble)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-inputs
  '("mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    "bvwbjplbgvbhsrlpgdmjqwftvncz"
    "nppdvjthqldpwncqszvftbrmjlhg"
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(define (tests)
  (in-test-suite ("Day 6: Tuning Trouble")
    (test-equal
        (map star-1 test-inputs)
        '(7 5 6 10 11))))




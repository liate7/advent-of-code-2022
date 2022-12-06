(define-module (day-6 tuning-trouble)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
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



(define (all-unique lst)
  (= (length lst)
     (length (delete-duplicates lst equal?))))

(define (start-of-packet-index buffer-list)
  (let rec ((buffer (drop buffer-list 4))
            (index 3)
            (last-4-chars (reverse (take buffer-list 4))))
    (if (all-unique last-4-chars)
        index
        (rec (cdr buffer)
             (1+ index)
             (take (cons (car buffer) last-4-chars)
                   4)))))

(define (star-1 buffer)
  (1+ (start-of-packet-index (string->list buffer))))

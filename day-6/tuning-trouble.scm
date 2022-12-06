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
        '(7 5 6 10 11))
    (test-equal
        (map star-2 test-inputs)
        '(19 23 23 29 26))))



(define (all-unique lst)
  (= (length lst)
     (length (delete-duplicates lst equal?))))

(define (first-n-unique-index n lst)
  (if (< (length lst) n) #f
      (let rec ((lst (drop lst n))
                (index (1- n))
                (last-n-chars (reverse (take lst n))))
        (cond ((all-unique last-n-chars)
               index)
              ((null? lst) #f)
              (else
               (rec (cdr lst)
                    (1+ index)
                    (take (cons (car lst) last-n-chars)
                          n)))))))

(define (start-of-packet-index buffer-list)
  (first-n-unique-index 4 buffer-list))

(define (star-1 buffer)
  (1+ (start-of-packet-index (string->list buffer))))



(define (start-of-message-index buffer-list)
  (first-n-unique-index 14 buffer-list))

(define (star-2 buffer)
  (1+ (start-of-message-index (string->list buffer))))

(define-module (advent-of-code-2022 day-4 camp-cleanup)
  #:use-module (advent-of-code-2022 utils)
  #:use-module (srfi srfi-1)
  #:use-module (advent-of-code-2022 test)
  #:use-module (ice-9 match)
  #:use-module (pipe))

(define test-input
  '("2-4,6-8"
    "2-3,4-5"
    "5-7,7-9"
    "2-8,3-7"
    "6-6,4-6"
    "2-6,4-8"
    ""))

(define (range->lset range-string)
  (match (map string->number (string-split range-string #\-))
    ((lower upper)
     (iota (1+ (- upper lower)) lower))))

(define (line->pair-assignment line)
  (map range->lset (string-split line #\,)))

(define* (pair-assignment-fully-contains pair #:key (equal equal?))
  (let ((l (first pair))
        (r (second pair)))
    (or (lset<= equal l r) (lset<= equal r l))))

(define (star-1 lines)
  (->> lines
       (remove string-null?)
       (map line->pair-assignment)
       (filter pair-assignment-fully-contains)
       (length)))



(define* (pair-assignment-redundant pair #:key (equal equal?))
  (not (null? (lset-intersection equal (first pair) (second pair)))))

(define (star-2 lines)
  (->> lines
       (remove string-null?)
       (map line->pair-assignment)
       (filter pair-assignment-redundant)
       (length)))

(define (tests)
  (in-test-suite ("Day 4: Camp Cleanup")
    (test-equal 2 (star-1 test-input))
    (test-equal 4 (star-2 test-input))))

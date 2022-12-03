(define-module (advent-of-code-2022 day-3 rucksack-reorganization)
  #:use-module (advent-of-code-2022 utils)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (pipe))

(define test-input
  '("vJrwpWtwJgWrhcsFMMfFFhFp"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
    "PmmdzqPrVvPwwTWBwg"
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
    "ttgJtRGJQctTZtZT"
    "CrZsJsPPZsGzwwsLwLmpwMDw"
    ""))

(define (line->rucksack line)
  (let ((line-list (string->list line))
        (len (string-length line)))
    (list (take line-list (/ len 2))
          (drop line-list (/ len 2)))))

(define (incorrectly-packed-items-type rucksack)
  (delete-duplicates
   (lset-intersection equal? (car rucksack) (cadr rucksack))
   equal?))

(define (item-priority item)
  (if (char-upper-case? item)
      (+ 27 (- (char->integer item) (char->integer #\A)))
      (+ 1  (- (char->integer item) (char->integer #\a)))))

(define (star-1 lines)
  (->> (remove string-null? lines)
       (map (compose incorrectly-packed-items-type
                     line->rucksack))
       (flatten)
       (map item-priority)
       (sum)))

(define group-rucksacks (curry segment 3))

(define (group-badge group)
  (delete-duplicates
   (apply lset-intersection equal?
          (map (curry apply append)
               group))
   equal?))

(define (star-2 lines)
  (->> (remove string-null? lines)
       (map line->rucksack)
       (group-rucksacks)
       (map group-badge)
       (flatten)
       (map item-priority)
       (sum)))

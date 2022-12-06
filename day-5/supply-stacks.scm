(define-module (day-5 supply-stacks)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-43)
  #:use-module (pipe)
  #:use-module (ice-9 receive))

(define debug #f)

(define test-input
  '("    [D]    "
    "[N] [C]    "
    "[Z] [M] [P]"
    " 1   2   3 "
    ""
    "move 1 from 2 to 1"
    "move 3 from 1 to 3"
    "move 2 from 2 to 1"
    "move 1 from 1 to 2"
    ""))

(define (tests)
  (in-test-suite ("Day 5: Supply Stacks")
    (test-equal "CMZ" (star-1 test-input))))



(define (cleanup-crate crate)
  (match crate
    ((#\[ char #\] #\space ...) char)
    ((or ()
         (#\space ...))
     #f)
    (_ (error "Invalid crate ~s" crate))))

(define (header->initial-state header)
  (let ((initial-stacks (car header))
        (data (cdr header)))
    (fold (lambda (row stacks)
            (vector-map (lambda (_ a stack)
                          (if a (cons a stack)
                              stack))
                        row stacks))
          initial-stacks
          data)))

(define (parse-original-state lines)
  "Generates a vector of stacks as described by the original state in the start of LINES."
  (define (read-header lines acc)
    (cond ((or (null? lines)
               (string-null? (car lines)))
           (values (if (null? acc) acc
                       (cons (make-vector (vector-length (car acc))
                                          '())
                             acc))
                   lines))
          ((string-index (car lines) #\[)
           (read-header (cdr lines)
                        (cons (list->vector
                               (string-transduce (compose (tsegment 4)
                                                          (tmap cleanup-crate))
                                                 rcons
                                                 (car lines)))
                              acc)))
          ((string-every (char-set-union char-set:digit char-set:whitespace)
                         (car lines))
           (values (cons (make-vector (->> (string-split (car lines) char-set:whitespace)
                                           (remove string-null?)
                                           (length))
                                      '())
                         acc)
                   (cdr lines)))))
  (receive (header rest) (read-header lines '())
    (values (header->initial-state header)
            rest)))

(define (crane-read line)
  (call-with-input-string (string-append "(" line ")")
    read))

(define (move stacks amount from to)
  (vector-map (lambda (i stack)
                (cond ((= i from) (drop stack amount))
                      ((= i to) (append (reverse
                                         (take (vector-ref stacks from)
                                               amount))
                                        stack))
                      (else stack)))
              stacks))

(define (crane-eval line stacks)
  (define (stack-n-exists n)
    (< (1- n) (vector-length stacks)))
  (match line
    (('move (? number? amount)
            'from (? number? (? stack-n-exists) from)
            'to   (? number? (? stack-n-exists) to))
     (when debug (format #t "~a~%" line))
     (move stacks amount (1- from) (1- to)))
    (() stacks)))

(define (crane-eval-lines lines stacks)
  (fold crane-eval stacks (map crane-read lines)))

(define (star-1 lines)
  (->> (receive (stacks lines) (parse-original-state lines)
         (crane-eval-lines lines stacks))
       (vector-map (lambda (i pair) (car pair)))
       (vector->list)
       (list->string)))

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
    (test-equal "CMZ" (star-1 test-input))
    (test-equal "MCD" (star-2 test-input))))



;;; Reading and parsing the header

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
  (define (read-crate-row line)
    (list->vector
     ;; Each crate is 3 characters wide, plus a space divider
     (string-transduce (compose (tsegment 4)
                                (tmap cleanup-crate))
                       rcons
                       line)))
  (define (label-number-of-stacks line)
    (->> (string-split line char-set:whitespace)
         (remove string-null?)
         (length)))
  (define (read-header lines acc)
    (cond ((or (null? lines)
               (string-null? (car lines)))
           (values (if (null? acc) acc
                       ;; If we saw crate rows without the bottom labels,
                       ;; just make one ourselves.
                       (cons (make-vector
                              (vector-length (car acc))
                              '())
                             acc))
                   lines))
          ;; If the line contains #\[, it's a crate row definition.
          ;; Return a vector of crates in this row.
          ((string-index (car lines) #\[)
           (read-header (cdr lines)
                        (cons (read-crate-row (car lines))
                              acc)))
          ;; If it's all numbers and whitespace, it's the bottom labels.
          ;; Make the initial vector of stacks
          ((string-every (char-set-union char-set:digit char-set:whitespace)
                         (car lines))
           (values (cons (make-vector
                          (label-number-of-stacks (car lines))
                          '())
                         acc)
                   (cdr lines)))))
  (receive (header rest) (read-header lines '())
    (values (header->initial-state header)
            rest)))

;;; Evaluating the instructions

(define (crane-read line)
  (call-with-input-string (string-append "(" line ")")
    read))

(define (move stacks amount from to)
  (define (move-mapper i stack)
    (cond ((= i from)
           (drop stack amount))
          ((= i to)
           (append ((if (cm9001?) identity reverse)
                    (take (vector-ref stacks from)
                          amount))
                   stack))
          (else stack)))
  (vector-map move-mapper stacks))

(define (crane-eval line stacks)
  (define (stack-n-exists n)
    (< (1- n) (vector-length stacks)))
  (match line
    (('move (? number? amount)
            'from (? (proc-and number? stack-n-exists) from)
            'to   (? (proc-and number? stack-n-exists) to))
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



(define cm9001? (make-parameter #f))

(define (star-2 lines)
  (->> (parameterize ((cm9001? #t))
         (receive (stacks lines) (parse-original-state lines)
           (crane-eval-lines lines stacks)))
       (vector-map (lambda (i pair) (car pair)))
       (vector->list)
       (list->string)))


;;; Objecty interlude

(define (^crane placement-orderer stacks)
  (define (stack-n-exists n)
    (< (1- n) (vector-length stacks)))
  (define self
	(match-lambda*
      (('move amount from to)
	   (let ((to-move (take (vector-ref stacks from) amount)))
         (vector-mod! stacks
                      from (λ (stack) (drop stack amount))
		              to (curry append (placement-orderer to-move)))
		 self))

      (('eval ('move (? number? amount)
                     'from (? (proc-and number? stack-n-exists) from)
                     'to   (? (proc-and number? stack-n-exists) to)))
       (self 'move amount (1- from) (1- to)))
      (('eval ())
       self)

      (('eval-lines lines)
       (for-each (compose (curry self 'eval)
                          crane-read)
                 lines)
       self)

      ;; To get desired result
	  ('(peek all)
	   (vector-map (λ (i stack) (car stack))
				   stacks))

      ;; Just for completeness
	  (('peek n)      (car (vector-ref stacks n)))
	  (('stack-state) (vector-copy stacks))

      (('pop n)
       (let ((top (car (vector-ref stacks n))))
         (vector-mod! stacks n cdr)
         top))))
  self)

(define ^cm9000 (curry ^crane reverse))
(define ^cm9001 (curry ^crane identity))

(define* (read-crane lines #:key (ctor ^cm9000))
  (receive (stacks rest) (parse-original-state lines)
    (values (ctor stacks)
            rest)))

(define (object-test ctor lines)
  (receive (crane lines) (read-crane lines #:ctor ctor)
    (crane 'eval-lines lines)
    (->> (crane 'peek 'all)
         (vector->list)
         (list->string))))

(define object-star-1 (curry object-test ^cm9000))
(define object-star-2 (curry object-test ^cm9001))

(define (object-tests)
  (in-test-suite ("Day 5: Supply Stacks")
    (test-equal "CMZ" (object-star-1 test-input))
    (test-equal "MCD" (object-star-2 test-input))))

(define-module (day-9 rope-bridge)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe)
  #:use-module (oop goops))

(define test-input
  '("R 4"
    "U 4"
    "L 3"
    "D 1"
    "R 4"
    "D 1"
    "L 5"
    "R 2"
    ""))

(define (tests)
  (in-test-suite ("Day 9: Rope Bridge")
    (test-equal (star-1 test-input) 13)))



(define-class <rope-sim> ()
  (locations #:init-form (make-hash-table) #:accessor locations)
  (head-at #:init-form '(0 0) #:accessor head-at)
  (tail-at #:init-form '(0 0) #:accessor tail-at))

(define (x point)
  (first point))
(define (y point)
  (second point))

(define* (make-rope-sim)
  (let ((ret (make <rope-sim>)))
    (hash-set! (locations ret) (tail-at ret) #t)
    ret))

(define (new-tail-position head-at tail-at)
  (define distance>1 (compose (curry < 1) abs))
  (match (map - head-at tail-at)
    ((0 (? distance>1 y))
     (map + tail-at (list 0 (sign y))))
    (((? distance>1 x) 0)
     (map + tail-at (list (sign x) 0)))
    (((? (negate distance>1)) (? (negate distance>1)))
     tail-at)
    ((x y)
     (map + tail-at (list (sign x) (sign y))))))

(define direction->move
  (match-lambda
    ('left '(-1 0))
    ('right '(1 0))
    ('up    '(0 1))
    ('down '(0 -1))
    ((and pt ((? number?) (? number?)))
     pt)))

(define-method (move-head! (rope <rope-sim>) direction)
  (let* ((move (direction->move direction))
         (new-head (map + (head-at rope) move))
         (new-tail (new-tail-position new-head (tail-at rope))))
    (set! (tail-at rope) new-tail)
    (hash-set! (locations rope) new-tail #t)
    (set! (head-at rope) new-head)))

(define (read-move line)
  (call-with-input-string (string-append "(" line ")")
    read))

(define-method (eval-line! (rope <rope-sim>) line)
  (define short-direction->long
    (match-lambda
      ('L 'left)
      ('R 'right)
      ('U 'up)
      ('D 'down)))
  (unless (string-null? line)
    (match-let ((((= short-direction->long dir) (? number? times))
                 (read-move line)))
      (repeat times (λ ()
                      (move-head! rope dir))))))

(define-method (eval-lines! (rope <rope-sim>) lines)
  (for-each (λ (l) (eval-line! rope l))
            lines))

(define-method (tail-visited-locations (rope <rope-sim>))
  (hash-fold (λ (key val acc)
               (if val (cons key acc)
                   acc))
             '()
             (locations rope)))

(define (render-rope rope)
  (match-let (((max-x max-y) (hash-fold
                              (λ (key val acc)
                                (map max key acc))
                              (map max (head-at rope) (tail-at rope) '(0 0))
                              (locations rope))))
    (for-each
     (λ (y)
       (for-each
        (λ (x)
          (cond ((equal? (list x y) (head-at rope))
                 (format #t "H"))
                ((equal? (list x y) (tail-at rope))
                 (format #t "T"))
                ((hash-ref (locations rope) (list x y))
                 (format #t "#"))
                (else (format #t "."))))
        (iota (1+ max-x)))
       (format #t "~%"))
     (iota (1+ max-y) max-y -1))
    (format #t "~%")))

(define (star-1 lines)
  (let ((rope (make-rope-sim)))
    (eval-lines! rope lines)
    (length (tail-visited-locations rope))))

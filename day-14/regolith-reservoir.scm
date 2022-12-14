(define-module (day-14 regolith-reservoir)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (pipe))

(define test-input
  '("498,4 -> 498,6 -> 496,6"
    "503,4 -> 502,4 -> 502,9 -> 494,9"
    ""))

(define (tests)
  (in-test-suite ("Day 14: Regolith Reservoir")
    (test-equal (star-1 test-input) 24)))



(define (point-ref grid pnt)
  (array-idx-ref grid pnt))

(define (read-comma-pair str)
  (map string->number (string-split str #\,)))

(define (read-path line)
  (map (compose read-comma-pair car)
       (-> (string-split line char-set:whitespace)
           (list-split (curry equal? "->")))))

(define (list-element-pairs lst)
  (if (< (length lst) 2)
      '()
      (cons (list (first lst) (second lst))
            (list-element-pairs (cdr lst)))))

(define (path->points path)
  (define (path->points-sum line acc)
    (append
     acc
     (match line
       (((x y1) (x y2))
        (map (curry list x)
             (inclusive-range y1 y2)))
       (((x1 y) (x2 y))
        (map (λ (x) (list x y))
             (inclusive-range x1 x2))))))
  (if (< (length path) 2) path
      (delete-duplicates
       (fold path->points-sum '()
             (list-element-pairs path)))))

(define (make-grid rock-points from)
  (let* ((points (cons from rock-points))
         (x-bounds (apply (juxt min max)
                          (map first points)))
         (y-bounds (apply (juxt min max)
                          (map second points)))
         (ret (make-array #f x-bounds y-bounds)))
    (array-index-map!
     ret
     (λ idx (and (member idx rock-points) 'rock)))
    ret))

(define (display-grid grid)
  (define grid-char
    (match-lambda
      ('rock #\#)
      (#f #\space)
      ('sand #\o)))
  (format #t "~{~{~a~}~%~}"
          (map (curry map grid-char)
               (apply zip (array->list grid)))))

(define (grid-empty-at? grid at)
  (and (apply array-in-bounds? grid at)
       (not (point-ref grid at))))

(define (attempted-next-points pnt)
  (map (curry map + pnt)
       '((0 1)    ; immediately below
         (-1 1)   ; below and to left
         (1 1)))) ; below and to right

(define (sand-step grid at)
  (define (try-step at)
    (cond ((not (apply array-in-bounds? grid at)) 'out-of-bounds)
          ((grid-empty-at? grid at) at)
          (else #f)))
  (if (equal? at 'out-of-bounds)
      'out-of-bounds
      (let rec ((attempts (attempted-next-points at)))
        (if (null? attempts) at
            (if-let (next (try-step (car attempts)))
                    next
                    (rec (cdr attempts)))))))

(define (drop-sand! grid start)
  (let ((resting-point (fixed-point (curry sand-step grid) start #:max-steps 1000)))
    (if (equal? resting-point 'out-of-bounds)
        #t
        (begin (apply array-set! grid 'sand resting-point)
               #f))))

(define start '(500 0))
(define (star-1 lines)
  (let* ((rocks (delete-duplicates
                 (concatenate (map (compose path->points read-path)
                                   (remove string-null? lines)))))
         (grid (make-grid rocks start)))
    (let rec ((grains 0))
      (if (drop-sand! grid start)
          grains
          (rec (1+ grains))))))

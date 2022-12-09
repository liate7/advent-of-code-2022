(define-module (day-8 treetop-tree-house)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 arrays)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-input
  '("30373"
    "25512"
    "65332"
    "33549"
    "35390"
    ""))

(define (tests)
  (in-test-suite ("Day 8: Treetop Tree House")
    (test-equal (star-1 test-input) 21)))



(define (digit->integer char)
  (if (char-set-contains? (string->char-set "0123456789") char)
      (- (char->integer char) (char->integer #\0))
      #f))

(define (read-tree-grid lines)
  (list->array 2 (map (compose (curry map digit->integer)
                               string->list)
                      (remove string-null? lines))))

(define (indices-for-direction direction index dimensions)
  (match direction
    ('left
     (map (curry list (car index))
          (iota (cadr index))))
    ('above
     (map (λ (x) (list x (cadr index)))
          (iota (car index))))
    ('right
     (map (curry list (car index))
          (iota (- (cadr dimensions) (cadr index) 1)
                (1+ (cadr index)))))
    ('below
     (map (λ (x) (list x (cadr index)))
          (iota (- (car dimensions) (car index) 1)
                (1+ (car index)))))))

(define (visible-from-direction tree-grid index direction)
  (let ((dimensions (array-dimensions tree-grid))
        (height-at (apply array-ref tree-grid index)))
    (every (lambda (index)
             (< (apply array-ref tree-grid index) height-at))
           (indices-for-direction direction index dimensions))))

(define (array-indices array)
  (let ((ret (array-copy array)))
    (array-index-map! ret
                      list)
    (->> ret
         (array->list)
         (flatten)
         (segment (array-rank array)))))

(define (array-map-indices proc array)
  (map proc (array-indices array)))

(define (star-1 lines)
  (let ((grid (read-tree-grid lines)))
    (->> grid
         (array-map-indices
          (lambda (idx)
            (any (curry visible-from-direction grid idx)
                 '(left right below above))))
         (filter identity)
         (length))))




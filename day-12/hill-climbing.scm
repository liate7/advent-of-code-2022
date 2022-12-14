(define-module (day-12 hill-climbing)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-11)
  #:use-module (pfds queues)
  #:use-module (pipe)
  #:use-module (fibers))

(define test-input
  '("Sabqponm"
    "abcryxxl"
    "accszExk"
    "acctuvwj"
    "abdefghi"
    ""))

(define (tests)
  (in-test-suite ("Day 12: Hill Climbing Algorithm")
    (test-equal (star-1 test-input) 31)
    (test-equal (star-2 test-input) 29)))



(define (read-grid lines)
  (list->array 2 (map string->list
                      (remove string-null? lines))))

(define-immutable-record-type <hill-climb>
  (%make-hill-climb grid queue marked end? should-enqueue?)
  hill-climb?
  (grid hill-grid)
  (queue hill-queue set-hill-queue)
  (marked hill-marked set-hill-marked)
  (end? hill-end-pred)
  (should-enqueue? hill-enqueue-pred))

(define (make-hill grid start end? enqueue?)
  (%make-hill-climb grid
                    (enqueue (make-queue)
                             (list start))
                    (list start)
                    end?
                    enqueue?))

(define (reachable? hill from to)
  (>= (1+ (char->height (array-idx-ref (hill-grid hill) from)))
      (char->height (array-idx-ref (hill-grid hill) to))))

(define (hill-marked? hill-climb idx)
  (member idx (hill-marked hill-climb)))

(define (make-hill-climb grid)
  (define (end? idx)
    (equal? (array-idx-ref grid idx) #\E))
  (define (enqueue? hill from to)
    (and (reachable? hill from to)
         (not (hill-marked? hill to))))
  (make-hill grid
             (array-index (curry equal? #\S) grid)
             end?
             enqueue?))

(set-record-type-printer!
 <hill-climb>
 (λ (hill port)
   (format port "#<hill-climb #:queue ~a #:marked ~a>"
           (map (λ (path) (list (car path) (length (cdr path))))
                (queue->list (hill-queue hill)))
           (hill-marked hill))))

(define (char->height char)
  (- (char->integer
      (match char
        (#\S #\a)
        (#\E #\z)
        (_ char)))
     (char->integer #\a)))

(define (array-adjacent-indices array idx)
  (define (mod-first proc)
    (list (proc (first idx)) (second idx)))
  (define (mod-second proc)
    (list (first idx) (proc (second idx))))
  (filter (curry apply array-in-bounds? array)
          (list idx
                (mod-first 1+) (mod-first 1-)
                (mod-second 1+) (mod-second 1-))))

(define (enqueue-all queue items)
  (fold (λ (item q) (enqueue q item))
        queue
        items))

(define (mark-all marked idxen)
  (apply lset-adjoin equal? marked idxen))

(define (hill-climb-step hill-climb)
  (when (queue-empty? (hill-queue hill-climb))
    (error "End is unreachable"))
  (receive (val queue) (dequeue (hill-queue hill-climb))
    (match-let*
        (((idx . path) val)
         (nexts (filter (curry (hill-enqueue-pred hill-climb)
                               hill-climb idx)
                        (array-adjacent-indices (hill-grid hill-climb)
                                                idx))))
      (values
       (set-fields hill-climb
         ((hill-queue)
          (enqueue-all queue
                       (map (λ (idx) (cons idx val)) nexts)))
         ((hill-marked)
          (mark-all (hill-marked hill-climb) nexts)))
       idx path))))

(define (hill-climb-path hill-climb)
  (let rec ((hill hill-climb))
    (receive (hill_ idx path) (hill-climb-step hill)
      (if ((hill-end-pred hill_) idx)
          (cons idx path)
          (rec hill_)))))

(define (star-1 lines)
  (1- (length
       (hill-climb-path
        (make-hill-climb (read-grid lines))))))

(define (make-hill-descent grid)
  (define (end? idx)
    (equal? (array-idx-ref grid idx) #\a))
  (define (enqueue? hill from to)
    (and (reachable? hill to from)
         (not (hill-marked? hill to))))
  (make-hill grid
             (array-index (curry equal? #\E) grid)
             end?
             enqueue?))

(define (star-2 lines)
  (1- (length
       (hill-climb-path
        (make-hill-descent (read-grid lines))))))

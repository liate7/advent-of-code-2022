(define-module (day-16 proboscidea-volcanium)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 copy-tree)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (pfds queues)
  #:use-module (pfds heaps)
  )

(define test-input
  '("Valve AA has flow rate=0; tunnels lead to valves DD, II, BB"
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA"
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB"
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE"
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD"
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG"
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH"
    "Valve HH has flow rate=22; tunnel leads to valve GG"
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ"
    "Valve JJ has flow rate=21; tunnel leads to valve II"
    ""))

(define (tests)
  (in-test-suite ("Day 16: Proboscidea Volcanium")
    (test-equal (star-1 test-input) 1651)))



(define-immutable-record-type <room>
  (make-room name flow-rate open? tunnels)
  room?
  (name room-name)
  (flow-rate room-flow-rate)
  (open? room-open? set-room-open)
  (tunnels room-tunnels))

(define (read-rate rate-str)
  (match (string-split rate-str #\=)
    (("rate" (= string->number (? number? rate)))
     rate)))

(define (read-valve valve-part)
  (match (string-split valve-part char-set:whitespace)
    (("Valve" name "has" "flow" rate)
     (list name (read-rate rate)))))

(define (read-tunnels tunnels-part)
  (match (string-split (string-trim tunnels-part) char-set:whitespace)
    (("tunnels" "lead" "to" "valves" tunnels ...)
     (map (λ (tunnel)
            (string->symbol (string-trim-right tunnel #\,)))
          tunnels))
    (("tunnel" "leads" "to" "valve" tunnel)
     (list (string->symbol tunnel)))))

(define (read-room line)
  (match (string-split line #\;)
    ((valve-part tunnels-part)
     (match-let (((((= string->symbol name) flow-rate) tunnels)
                  (list (read-valve valve-part) (read-tunnels tunnels-part))))
       (cons name (make-room name flow-rate #f tunnels))))
    (("")
     '())))

(define (create-rooms-graph lines)
  (remove null? (map read-room lines)))

(define (rooms-graph-ref rooms place)
  (assoc-ref rooms place))

(define (rooms-all rooms)
  (map cdr rooms))

(define (room-open-valve rooms place)
  (assoc-update rooms place
                (λ (room)
                  (set-room-open room #t))))

(define (room-actions room)
  ((if (room-open? room)
       (arg 1)
       cons)
   'open
   (room-tunnels room)))

(define (dist-name-order l r)
  (< (car l) (car r)))

(define (weight rooms _place to)
  1
  #;(room-flow-rate (rooms-graph-ref rooms to)))

(define (dijkstra-relax rooms from dist heap edge-to dist-to)
  (define relax-sum 
    (match-lambda*
      ((node (heap edge-to dist-to))
       (let* ((recorded-dist-to-node (assoc-ref dist-to node))
              (known-dist-to-node (+ (weight rooms from node) dist)))
         (if (< known-dist-to-node recorded-dist-to-node)
             (list (list->heap (cons (list known-dist-to-node node)
                                     (remove (λ (x) (equal? (second x) node))
                                             (heap->list heap)))
                               dist-name-order)
                   (assoc-set edge-to node from)
                   (assoc-set dist-to node known-dist-to-node))
             (list heap edge-to dist-to))))))
  (fold relax-sum
        (list heap edge-to dist-to)
        (room-tunnels (rooms-graph-ref rooms from))))

(define (dijkstra-shortest-path rooms start)
  (let rec ((heap (heap dist-name-order (list 0 start)))
            (edge-to '())
            (dist-to (assoc-set (map (λ (room)
                                       (cons (room-name room) most-positive-fixnum))
                                     (rooms-all rooms))
                                start 0)))
    (if (heap-empty? heap)
        (values edge-to dist-to)
        (receive (cur heap_) (heap-pop heap)
          (apply rec (dijkstra-relax rooms (second cur) (first cur)
                                     heap_ edge-to dist-to))))))

;; I don't understand why this works for the test input =\
(define (dijkstra-next-to-open rooms from minutes-left)
  (receive (edges distances) (dijkstra-shortest-path rooms from)
    (map car
         (sort
          (map (λ (p)
                 (cons (car p)
					   (cond ((room-open? (rooms-graph-ref rooms (car p)))
                              0)
                             ((zero? (cdr p))
                              (* 2 (room-flow-rate (rooms-graph-ref rooms (car p)))))
                             (else
                              (/ (room-flow-rate (rooms-graph-ref rooms (car p)))
                                 (cdr p))))))
		       distances)
          (λ (l r) (> (cdr l) (cdr r)))))))

(define shortest-paths
  (memoized-lambda (rooms from)
    (let rec ((queue (enqueue (make-queue) from))
              (marked (list from))
              (edge-to '()))
      (if (queue-empty? queue) edge-to
          (let*-values (((node dequeued) (dequeue queue))
                        ((room) (rooms-graph-ref rooms node))
                        ((unmarked-nexts)
                         (remove (λ (node) (member node marked))
                                 (room-tunnels room))))
            (rec (enqueue-all dequeued unmarked-nexts)
                 (append unmarked-nexts marked)
                 (append (map (curry xcons node) unmarked-nexts)
                         edge-to)))))))

(define (path-length shortests from to)
  (let rec ((to to)
            (acc 0))
    (cond ((equal? from to) acc)
          ((not to) #f)
          (else (rec (assoc-ref shortests to) (1+ acc))))))

(define (path-next shortests from to)
  (let ((next (assoc-ref shortests to)))
    (if (equal? next from) to
        (path-next shortests from next))))

(define (room-best-action rooms place minutes-left start)
  (let* ((shortests (shortest-paths rooms place))
         (dest (first (dijkstra-next-to-open rooms place minutes-left))))
    (cond ((zero? minutes-left) 'wait)
          #;((and (> (room-flow-rate (rooms-graph-ref rooms place)))
                (not (room-open? (rooms-graph-ref rooms place))))
           'open)
          ((null? dest) 'wait)
          ((equal? dest place) 'open)
          (else (path-next shortests place dest)))))

(define (rooms-turn rooms place minutes-left start)
  (if (zero? minutes-left)
      (values rooms 0 place 'wait)
      (match (room-best-action rooms place minutes-left start)
        ('open
         (values (room-open-valve rooms place)
                 (* (1- minutes-left)
                    (room-flow-rate (rooms-graph-ref rooms place)))
                 place
                 'open))
        ('wait
         (values rooms 0 place 'wait))
        (path
         (values rooms 0 path path)))))

(define* (rooms-best-score rooms #:key (start 'AA) (minutes-left 30))
  (let rec ((rooms rooms)
            (score 0)
            (minutes-left minutes-left)
            (place start)
            (log '()))
    (if (zero? minutes-left) (values score place (reverse log))
        (receive (rooms score-added new-place move)
            (rooms-turn rooms place minutes-left start)
          (rec rooms (+ score score-added) (1- minutes-left) new-place
               (cons move
                     log))))))

(define (star-1 lines)
  (rooms-best-score (create-rooms-graph lines)))

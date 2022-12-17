(define-module (day-15 beacon-exclusion-zone)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-41)
  #:use-module (pipe)
  #:use-module (pfds hamts))

(define test-input
  '("Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16"
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3"
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16"
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16"
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16"
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10"
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10"
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10"
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17"
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22"
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3"
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3"
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3"
    ""))

(define (tests)
  (in-test-suite ("Day 15: Beacon Exclusion Zone")
    (test-equal (star-1 test-input 10) 26)
    (test-equal (star-2 test-input (make-point 20 20)) 56000011)))



(define-immutable-record-type <point>
  (make-point x y)
  point?
  (x point-x set-point-x)
  (y point-y set-point-y))

(define (display-point point port)
  (format port "#<point ~a ~a>"
          (point-x point)
          (point-y point)))

(set-record-type-printer! <point> display-point)

(define (read-place str name)
  (match (string-split (string-trim-right str #\,) #\=)
    ((name (= string->number (? number? val)))
     val)))

(define (read-position str prefix)
  (when (not (string-prefix? prefix str))
    (error "Wrong prefix: input ~s, should have been ~s" str prefix))
  (match (remove null? (string-split (substring str (string-length prefix))
                                     char-set:whitespace))
    ((_ ... "at" x-loc-str y-loc-str)
     (make-point (read-place x-loc-str "x")
                 (read-place y-loc-str "y")))))

(define (read-sensor-report line)
  (match (string-split line #\:)
    ((sensor-position closest-beacon)
     (list (read-position sensor-position "Sensor")
           (read-position closest-beacon " closest beacon")))
    (("")
     '())))

(define (manhattan-distance l r)
  (match (list l r)
    ((($ <point> lx ly) ($ <point> rx ry))
     (+ (abs (- rx lx)) (abs (- ry ly))))
    (_
     (error "Type error: ~{~a is not a point. ~}"
            (remove point? (list l r))))))

(define* (create-beacon-hamt hamt)
  (define (reverse-sum key val hamt)
    (hamt-set hamt val key))
  (hamt-fold reverse-sum (make-hamt-from) hamt))

(define (sensor-too-close? sensor closest place)
  (<= (manhattan-distance sensor place)
      (manhattan-distance sensor closest)))

(define (any-sensors-too-close? sensor-closest-beacon place)
  (define too-close-sum
    (match-lambda*
      (((s . b) acc)
       (if acc acc
           (and (not (equal? place b))
                (not (equal? place s))
                (<= (manhattan-distance s place)
                    (manhattan-distance s b)))))))
  (fold too-close-sum #f sensor-closest-beacon))

(define (beacons-sum key val acc)
  (match-let
      (((($ <point> key-x key-y)
         max-x min-x)
        (cons key acc))
       (dist (manhattan-distance key val)))
    (list (max (+ key-x dist) max-x) (min (- key-x dist) min-x))
    ))

(define (marked-along-y sensor-closest-beacon y)
  (match-let*
      (((min-x max-x)
        (hamt-fold beacons-sum
                   (list most-negative-fixnum most-positive-fixnum)
                   sensor-closest-beacon)))
    (count (curry any-sensors-too-close? (hamt->alist sensor-closest-beacon))
           (map (λ (x) (make-point x y))
                (inclusive-range min-x 
                                 max-x)))))

(define* (star-1 lines #:optional (line 2000000))
  (let* ((sensor-closest-beacon
          (->> (map read-sensor-report lines)
               (concatenate)
               (apply make-hamt-from))))
    (marked-along-y sensor-closest-beacon line)))

(define (beacon-tuning-frequency position)
  (+ (* (point-x position) 4000000)
     (point-y position)))

(define (manhattan-circle from radius)
  (define (all-quadrants pts)
    (append pts
            (map (λ (pt)
                   (set-point-x pt (- (point-x pt))))
                 pts)
            (map (λ (pt)
                   (set-point-y pt (- (point-y pt))))
                 pts)
            (map (λ (pt)
                   (make-point (- (point-x pt))
                               (- (point-y pt))))
                 pts)))
  (map (λ (pt)
         (make-point (+ (point-x pt) (point-x from))
                     (+ (point-y pt) (point-y from))))
       (all-quadrants
        (map make-point
             (iota (1+ radius))
             (iota (1+ radius) (1+ radius) -1)))))

(define (ineligible-pos? sensors pt)
  (any (match-lambda
         ((s . dist)
          (<= (manhattan-distance s pt)
              dist)))
       sensors))

(define (distress-beacon-position sensors size)
  (define (in-area? pt)
    (and (<= 0 (point-x pt) (point-x size))
         (<= 0 (point-y pt) (point-y size))))
  (match-let rec ((((s . dist) rest ...) sensors))
    (if-let (pos (find (negate (curry ineligible-pos? sensors))
                       (filter in-area?
                               (manhattan-circle s dist))))
            pos
            (if (null? rest) #f
                (rec rest)))))

(define* (star-2 lines #:optional (size (make-point 4000000 4000000)))
  (let* ((sensor-closests
          (->> (map read-sensor-report lines)
               (remove null?)
               (map (juxt first (curry apply manhattan-distance)))
               (map (curry apply cons)))))
    (beacon-tuning-frequency (distress-beacon-position sensor-closests size))))

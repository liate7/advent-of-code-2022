(define-module (day-13 distress-signal)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 control)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-input
  '("[1,1,3,1,1]"
    "[1,1,5,1,1]"
    ""
    "[[1],[2,3,4]]"
    "[[1],4]"
    ""
    "[9]"
    "[[8,7,6]]"
    ""
    "[[4,4],4,4]"
    "[[4,4],4,4,4]"
    ""
    "[7,7,7,7]"
    "[7,7,7]"
    ""
    "[]"
    "[3]"
    ""
    "[[[]]]"
    "[[]]"
    ""
    "[1,[2,[3,[4,[5,6,7]]]],8,9]"
    "[1,[2,[3,[4,[5,6,0]]]],8,9]"
    ""))

(define (tests)
  (in-test-suite ("Day 13: Distress Signal")
    (test-equal (star-1 test-input) 13)
    (test-equal (star-2 test-input) 140)))



(define (read-list lst acc)
  (match lst
    ((#\[ . rest)
     (receive (sublst rest_) (read-list rest '())
       (read-list rest_ (cons sublst acc))))
    (((? (curry char-set-contains? char-set:digit)) . _)
     (receive (num rest) (read-num lst 0)
       (read-list rest (cons num acc))))
    (((? (curry char-set-contains?
                (char-set-adjoin char-set:whitespace #\,)))
      . rest)
     (read-list rest acc))
    ((#\] . rest)
     (values (reverse acc) rest))
    (()
     (values (reverse acc) '()))))

(define (read-num lst acc)
  (cond ((null? lst)
         (values acc '()))
        ((char-set-contains? char-set:digit (car lst))
         (read-num (cdr lst)
                   (+ (* 10 acc)
                      (string->number (string (car lst))))))
        (else
         (values acc lst))))

(define (read-packet line)
  (let ((lst (string->list line)))
    (match lst
      (() #f)
      ((#\[ . rest)
       (read-list rest '()))
      (_
       (read-num lst 0)))))

(define (pair-packets packets)
  (list-split packets (negate identity)))

(define (packet< packet . packets)
  (define (pair< l r)
    (let/ec return
      (let rec ((l l)
                (r r))
        (match (list l r)
          (((? number?) (? number?))
           (cond ((< l r) (return #t))
                 ((> l r) (return #f))
                 ((= l r) 'who-knows)))

          (((? list?) (? list?))
           (for-each rec l r)
           (rec (length l) (length r)))

          (((? number?) (? list?))
           (rec (list l) r))
          (((? list?) (? number?))
           (rec l (list r)))))))

  (if (null? packets) #t
      (and (pair< packet (car packets))
           (apply packet< packets))))

(define (star-1 lines)
  (->> (map read-packet lines)
       (pair-packets)
       (remove null?)
       (list-indexes (curry apply packet<))
       (map 1+)
       (sum)))

(define (sort-packets packets)
  (sort (cons* '((2))
               '((6))
               packets)
        packet<))

(define (message-decoder-key sorted-packets)
  (* (1+ (list-index (curry equal? '((2)))
                     sorted-packets))
     (1+ (list-index (curry equal? '((6)))
                     sorted-packets))))

(define (star-2 lines)
  (->> (map read-packet lines)
       (remove (negate identity))
       (sort-packets)
       (message-decoder-key)))

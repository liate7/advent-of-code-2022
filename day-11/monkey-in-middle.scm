(define-module (day-11 monkey-in-middle)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (pipe)
  #:use-module (oop goops))

(define test-input
  '("Monkey 0:"
   "  Starting items: 79, 98"
   "  Operation: new = old * 19"
   "  Test: divisible by 23"
   "    If true: throw to monkey 2"
   "    If false: throw to monkey 3"
   ""
   "Monkey 1:"
   "  Starting items: 54, 65, 75, 74"
   "  Operation: new = old + 6"
   "  Test: divisible by 19"
   "    If true: throw to monkey 2"
   "    If false: throw to monkey 0"
   ""
   "Monkey 2:"
   "  Starting items: 79, 60, 97"
   "  Operation: new = old * old"
   "  Test: divisible by 13"
   "    If true: throw to monkey 1"
   "    If false: throw to monkey 3"
   ""
   "Monkey 3:"
   "  Starting items: 74"
   "  Operation: new = old + 3"
   "  Test: divisible by 17"
   "    If true: throw to monkey 0"
   "    If false: throw to monkey 1"
   ""))

(define (tests)
  (in-test-suite ("Day 11: Monkey in the Middle")
    (test-equal (star-1 test-input) 10605)))



(define (split-monkey-entries lines)
  (remove null? (list-split lines string-null?)))

(define (parse-monkey-number line)
  (match (string-split (string-trim-right line #\:) #\space)
    (("Monkey" (= string->number (? number? num)))
     num)))

(define (parse-monkey-items line)
  (match-let ((("Starting items" items) (map string-trim (string-split line #\:))))
    (->> (string-split items (char-set #\, #\space))
         (remove string-null?)
         (map string->number))))

(define op-proc
  (match-lambda ("*" *) ("/" /) ("+" +) ("-" -)))

(define (parse-monkey-operation line)
  (match-let ((("Operation" operation) (map string-trim (string-split line #\:))))
    (match (string-split operation #\space)
      (("new" "=" lh op rh)
       (let ((proc (op-proc op)))
         (lambda (old)
           (proc (or (string->number lh) old)
                 (or (string->number rh) old))))))))

(define (parse-monkey-test line)
  (match (string-split (string-trim line) #\space)
    (("Test:" "divisible" "by" (= string->number (? number? num)))
     (lambda (worry)
       (zero? (modulo worry num))))))

(define (parse-monkey-throw line case)
  (match (string-split (string-trim line) #\space)
    (("If" (? (curry equal? case)) "throw" "to" "monkey"
      (= string->number (? number? num)))
     num)))

(define (parse-monkey-action lines)
  (match-let* (((test when-true when-false) lines)
               (pred (parse-monkey-test test))
               (true-recv (parse-monkey-throw when-true "true:"))
               (false-recv (parse-monkey-throw when-false "false:")))
    (list pred true-recv false-recv)))

(define (parse-monkey-entry game entry)
  (match entry
    ((number starting-items operation test ...)
     (apply add-monkey! game
            (parse-monkey-number number)
            (parse-monkey-items starting-items)
            (parse-monkey-operation operation)
            (parse-monkey-action test)))))

(define-class <monkey> ()
  (items #:accessor items
         #:init-keyword #:items
         #:init-form '())
  (worry-change #:getter worry-change
                #:init-keyword #:worry-change)
  (test #:getter test
        #:init-keyword #:test)
  (true-recv  #:accessor true-recv
              #:init-form #f)
  (false-recv #:accessor false-recv
              #:init-form #f)
  (inspections #:accessor inspections
               #:init-form 0))

(define-method (catch-item! (monkey <monkey>) num)
  (set! (items monkey) (append (items monkey) (list num))))

(define-method (do-turn! (monkey <monkey>))
  (define (handle-item item)
    (set! (inspections monkey) (1+ (inspections monkey)))
    (let* ((worry ((worry-change monkey) item))
           (worry (floor/ worry 3)))
      (catch-item! (if ((test monkey) worry)
                       (true-recv  monkey)
                       (false-recv monkey))
                   worry)))
  (let ((items_ (items monkey)))
    (set! (items monkey) '())
    (for-each handle-item items_)))

(define-method (write (monkey <monkey>) port)
  (format port "#<monkey items: ~a inspections: ~a>" (items monkey) (inspections monkey)))

(define-class <game> ()
  (monkeys      #:accessor monkeys
                #:init-form (make-hash-table))
  (dependencies #:accessor dependencies
                #:init-form (make-hash-table)))

(define-method (add-monkey! (game <game>) number items operation test when-true when-false)
  (define (handle-dependencies n when-true when-false)
    (let ((new (hash-ref (monkeys game) n)))
      ;; If already constructed, just add the receiver
      (when-let (when-true-recv (hash-ref (monkeys game) when-true))
          (set! (true-recv new) when-true-recv))
      (when-let (when-false-recv (hash-ref (monkeys game) when-false))
          (set! (false-recv new) when-false-recv))
      ;; Add dependencies, for if not constructed (or replaced)
      (hash-mod! (dependencies game)
                 when-true  (λ (val)
                              (if val (cons (list #t new) val)
                                  (list (list #t new))))
                 when-false (λ (val)
                              (if val (cons (list #f new) val)
                                  (list (list #f new)))))
      ;; Resolve dependencies waiting on us
      (when-let (dep-on (hash-ref (dependencies game) n))
          (for-each (match-lambda ((#t to) (set! (true-recv  to) new))
                                  ((#f to) (set! (false-recv to) new)))
                    dep-on))))
  (let ((new (make <monkey> #:items items #:worry-change operation #:test test)))
    (hash-set! (monkeys game) number new)
    (handle-dependencies number when-true when-false)
    new))

(define-method (do-turn! (game <game>))
  (for-each (λ (idx) (do-turn! (hash-ref (monkeys game) idx)))
            (sort (hash-table-keys (monkeys game)) <)))

(define-method (monkey-inspections (game <game>))
  (hash-map->list (λ (key val) (cons key (inspections val)))
                  (monkeys game)))

(define-method (write (game <game>) port)
  (format port "#<monkeys~{ ~a~}>"
          (map (λ (idx) (list idx (hash-ref (monkeys game) idx)))
               (sort (hash-table-keys (monkeys game)) <))))

(define (monkey-business game)
  (prod (take (sort (map cdr (monkey-inspections game)) >) 2)))

(define (star-1 lines)
  (let ((game (make <game>)))
    (for-each (curry parse-monkey-entry game)
              (split-monkey-entries lines))
    (repeat 20 (λ () (do-turn! game)))
    (monkey-business game)))

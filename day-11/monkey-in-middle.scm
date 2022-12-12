(define-module (day-11 monkey-in-middle)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-17)
  #:use-module (srfi srfi-43)
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
    (test-equal (star-1 test-input) 10605)
    (test-equal (star-2 test-input) 2713310158)))



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
     num)))

(define (parse-monkey-throw line case)
  (match (string-split (string-trim line) #\space)
    (("If" (? (curry equal? case)) "throw" "to" "monkey"
      (= string->number (? number? num)))
     num)))

(define (parse-monkey-action lines)
  (match-let* (((test when-true when-false) lines)
               (divisor (parse-monkey-test test))
               (true-recv (parse-monkey-throw when-true "true:"))
               (false-recv (parse-monkey-throw when-false "false:")))
    (list divisor true-recv false-recv)))

(define (parse-monkey-entry entry)
  (match entry
    ((_number starting-items operation test ...)
     (apply make-monkey
            (parse-monkey-items starting-items)
            (parse-monkey-operation operation)
            0
            (parse-monkey-action test)))))

(define-record-type <monkey>
  (make-monkey items worry-change
               inspections
               test-divisor true-recv false-recv)
  monkey?
  (items monkey-items set-monkey-items!)
  (worry-change monkey-worry-change)
  (test-divisor monkey-test-divisor)
  (true-recv monkey-true-recv)
  (false-recv monkey-false-recv)
  (inspections monkey-inspections set-monkey-inspections!))

(set-record-type-printer! <monkey>
                          (λ (m port)
                            (format port "#<monkey items: ~a test-divisor: ~a inspections ~a>"
                                    (monkey-items m)
                                    (monkey-test-divisor m)
                                    (monkey-inspections m))))

(define-class <game> ()
  (monkeys #:getter monkeys
           #:init-keyword #:monkeys)
  (mod-by  #:getter mod-by
           #:init-keyword #:mod-by))

(define (make-game monkeys)
  (make <game> #:monkeys (list->vector monkeys)
        #:mod-by (prod (map monkey-test-divisor monkeys))))

(define-method (do-turn! monkey (game <game>))
  (define (process-item item)
    (let* ((worry ((worry-reduction)
                   ((monkey-worry-change monkey) item)))
           (recv (vector-ref (monkeys game)
                             (if (zero? (modulo worry (monkey-test-divisor monkey)))
                                 (monkey-true-recv monkey)
                                 (monkey-false-recv monkey)))))
      (set-monkey-items! recv
                         (cons (modulo worry (mod-by game))
                               (monkey-items recv)))
      (set-monkey-inspections! monkey (1+ (monkey-inspections monkey)))))
  (for-each process-item
            (monkey-items monkey))
  (set-monkey-items! monkey '()))

(define-method (do-turn! (game <game>))
  (vector-for-each (λ (i m) (do-turn! m game))
                   (monkeys game)))

(define-method (inspections (game <game>))
  (map monkey-inspections
       (vector->list (monkeys game))))

(define-method (write (game <game>) port)
  (format port "#<monkeys~{ ~a~}>"
          (vector->list (vector-map list (monkeys game)))))

(define (monkey-business game)
  (prod (take (sort (inspections game) >) 2)))

(define (star-1 lines)
  (monkey-business-after-n-turns 20 lines))



(define worry-reduction (make-parameter (λ (worry) (floor/ worry 3))))

(define (star-2 lines)
  (parameterize ((worry-reduction identity))
    (monkey-business-after-n-turns 10000 lines)))

(define (monkey-business-after-n-turns n lines)
  (let ((game (make-game (map parse-monkey-entry
                              (split-monkey-entries lines)))))
    (repeat n (λ () (do-turn! game)))
    (monkey-business game)))

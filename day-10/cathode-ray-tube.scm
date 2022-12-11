(define-module (day-10 cathode-ray-tube)
  #:use-module (test)
  #:use-module (utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define test-inputs
  '(("noop"
     "addx 3"
     "addx -5")
    ("addx 15" "addx -11" "addx 6" "addx -3" "addx 5" "addx -1" "addx -8" "addx 13" "addx 4"
     "noop" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1" "addx 5" "addx -1"
     "addx -35" "addx 1" "addx 24" "addx -19" "addx 1" "addx 16" "addx -11" "noop" "noop" "addx 21"
     "addx -15" "noop" "noop" "addx -3" "addx 9" "addx 1" "addx -3" "addx 8" "addx 1" "addx 5"
     "noop" "noop" "noop" "noop" "noop" "addx -36" "noop" "addx 1" "addx 7" "noop" "noop" "noop"
     "addx 2" "addx 6" "noop" "noop" "noop" "noop" "noop" "addx 1" "noop" "noop" "addx 7" "addx 1"
     "noop" "addx -13" "addx 13" "addx 7" "noop" "addx 1" "addx -33" "noop" "noop" "noop" "addx 2"
     "noop" "noop" "noop" "addx 8" "noop" "addx -1" "addx 2" "addx 1" "noop" "addx 17" "addx -9"
     "addx 1" "addx 1" "addx -3" "addx 11" "noop" "noop" "addx 1" "noop" "addx 1" "noop" "noop"
     "addx -13" "addx -19" "addx 1" "addx 3" "addx 26" "addx -30" "addx 12" "addx -1" "addx 3"
     "addx 1" "noop" "noop" "noop" "addx -9" "addx 18" "addx 1" "addx 2" "noop" "noop" "addx 9"
     "noop" "noop" "noop" "addx -1" "addx 2" "addx -37" "addx 1" "addx 3" "noop" "addx 15"
     "addx -21" "addx 22" "addx -6" "addx 1" "noop" "addx 2" "addx 1" "noop" "addx -10" "noop"
     "noop" "addx 20" "addx 1" "addx 2" "addx 2" "addx -6" "addx -11" "noop" "noop" "noop" "")))

(define (tests)
  (in-test-suite ("Day 10: Cathode-Ray Tube")
    (test-equal (star-1 (second test-inputs)) 13140)))



(define (read-instruction line)
  (call-with-input-string (string-append "(" line ")")
    read))

(define* (eval-instruction instr #:optional (signal-hist '(1)))
  (let ((cur (car signal-hist)))
    (match instr
      (('addx (? number? num))
       (cons* (+ cur num) cur signal-hist))
      (('noop)
       (cons cur signal-hist))
      (() signal-hist))))

(define (signal-history instrs)
  (reverse (fold eval-instruction '(1) instrs)))

(define (every-n-from from step lst)
  (if (>= (length lst) from)
      (let rec ((cur (list-cdr-ref lst from))
                (n (+ step from))
                (acc (list (* from (list-ref lst (1- from))))))
        (if (>= (length cur) step)
            (rec (list-cdr-ref cur step)
                 (+ n step)
                 (cons (* n (list-ref cur (1- step)))
                       acc))
            (reverse acc)))))

(define (star-1 lines)
  (->> lines
       (map read-instruction)
       (signal-history)
       (every-n-from 20 40)
       (sum)))

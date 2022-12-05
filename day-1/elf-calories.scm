(define-module (day-1 elf-calories)
  #:use-module (utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define (read-elves port)
  (define (lines->elves-sum line elves)
    (cond ((string-null? line)
           (cons* '()                   ; next elf
                  (car elves)           ; just finished elf
                  (cdr elves)))         ; older elves
          (else (cons (cons (string->number line) (car elves)) (cdr elves)))))
  (drop-right
   (->> (string-split (get-string-all port) #\newline)
        (fold-right lines->elves-sum '(())))
   1))

(define (elves-by-most-calories elf-list)
  (sort elf-list (lambda (l r)
                   (> (apply + l) (apply + r)))))

(define (star-1 elf-list)
  (apply + (car (elves-by-most-calories elf-list))))

(define (star-2 elf-list)
  (apply +
   (flatten (take (elves-by-most-calories elf-list)
                  3))))

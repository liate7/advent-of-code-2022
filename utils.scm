(define-module (utils)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (pipe)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-171))

(define-public (flatten lst)
  (list-transduce tflatten rcons lst))

(define-public (get-all-lines port)
  (string-split
   (get-string-all port)
   #\newline))

(define-public (sum lst)
  (apply + lst))

(define-public (prod lst)
  (apply * lst))

(define-public (curry proc . original)
  (lambda after
    (apply proc (append original after))))

(define-public (segment n lst)
  "Break up a list in to segments of N, based on `tsegment'"
  (list-transduce (tsegment n) rcons lst))

(define-public (proc-and . procs)
  "Returns a predicate which is true if all members of PROCS are true with the inputs.

The resulting function is properly short-circuiting, like normal and."
  (lambda args
    (let rec ((procs procs))
      (or (null? procs)
          (and (apply (car procs) args)
               (rec (cdr procs)))))))

(export if-let)
(define-syntax-rule (if-let (var form) then else)
  (let ((var form))
    (if var
        then
        else)))

(export when-let)
(define-syntax-rule (when-let (var form) body ...)
  (let ((var form))
    (when var
      body ...)))

(define-public (assoc-set alist key val)
  (alist-cons key val
              (remove (compose (curry equal? key)
                               car)
                      alist)))

(define-public plist->alist
  (match-lambda
    ((key val . rest)
     (alist-cons key val
                 (if (< (length rest) 2)
                     '()
                     (plist->alist rest))))))

(define-public (vector-mod! vect i proc . rest)
  (vector-set! vect i
               (proc (vector-ref vect i)))
  (if (null? rest) vect
      (apply vector-mod! vect rest)))

(define-public (vector-mod vect i proc . rest)
  (if (null? rest)
      (vector-map (lambda (index val)
                    (if (= index i)
                        (proc val)
                        val))
                  vect)
      (let ((index-functions
             (alist-cons i proc
                         (plist->alist rest))))
        (vector-map (lambda (index val)
                      (if-let (proc (assoc-ref index-functions index))
                              (proc val)
                              val))
                    vect))))

(define-public (array-indices array)
  (let ((ret (array-copy array)))
    (array-index-map! ret
                      list)
    (->> ret
         (array->list)
         (flatten)
         (segment (array-rank array)))))

(define-public (sign num)
  (cond ((> num 0)   1)
        ((< num 0)  -1)
        ((zero? num) 0)))

(define-public (repeat n thunk)
  (for-each (λ (_ignore) (thunk))
            (iota n)))

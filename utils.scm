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

(define-public (find-duplicates lst)
  (define duplicates-sum
    (match-lambda*
      ((item #(duplicates seen))
       (if (member item seen)
           (vector (lset-adjoin equal? duplicates item) seen)
           (vector duplicates (cons item seen))))))
  (vector-ref (fold duplicates-sum #(() ()) lst)
              0))


(define-public (list-split lst pred)
  (define (splitter-sum val acc)
    (if (pred val)
        (cons* '() (reverse (car acc)) (cdr acc))
        (cons (cons val (car acc)) (cdr acc))))
  (reverse (fold splitter-sum '(()) lst)))

(define-public (hash-mod! table key proc . rest)
  (hash-set! table key
               (proc (hash-ref table key)))
  (if (null? rest) table
      (apply hash-mod! table rest)))

(define-public (hash-table-keys table)
  (hash-fold (λ (key _vals acc) (cons key acc))
             '()
             table))

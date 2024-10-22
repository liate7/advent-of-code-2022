(define-module (utils)
  #:use-module (ice-9 arrays)
  #:use-module (ice-9 control)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 textual-ports)
  #:use-module (pfds hamts)
  #:use-module (pipe)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-43)
  #:use-module (srfi srfi-41)
  #:use-module (pfds queues))

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

(define-public (proc-and proc . procs)
  "Returns a predicate which is true if all members of PROCS are true with the inputs.

The resulting function is properly short-circuiting, like normal and."
  (lambda args
    (let rec ((proc proc)
              (procs procs))
      (if (null? procs)
          (apply proc args)
          (and (apply proc args)
               (rec (car procs) (cdr procs)))))))

(define-public ((proc-or proc . procs) . args)
  (let rec ((proc proc)
            (procs procs))
    (if (null? procs)
        (apply proc args)
        (or (apply proc args)
            (rec (car procs) (cdr procs))))))

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
  (let ((k-v (cons key val)))
    (let rec ((cur alist)
              (acc '())
              (found? #f))
      (cond ((and (null? cur) found?)
             (reverse acc))
            ((null? cur)
             (cons k-v (reverse acc)))
            ((equal? (caar cur) key)
             (rec (cdr cur) (cons k-v acc) #t))
            (else
             (rec (cdr cur) (cons (car cur) acc) found?))))))

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

(define-public (array-index pred array)
  (let/ec ret
   (for-each (λ (idx)
               (when (pred (array-idx-ref array idx))
                 (ret idx)))
             (array-indices array))
   #f))

(define-public (tuples-in lst . lsts)
  (define (tuples-sum item res)
    (append (map (curry cons item)
                 (apply tuples-in lsts))
            res))
  (if (null? lsts)
      (map list lst)
      (fold tuples-sum '() lst)))

(define-public (array-idx-ref array idx)
  (apply array-ref array idx))

(define-public (list-indexes pred lst . lsts)
  (let ((first-idx (apply list-index pred lst lsts)))
    (if first-idx
        (cons first-idx
              (map (curry + 1 first-idx)
                   (apply list-indexes pred
                          (map (λ (l) (list-cdr-ref l (1+ first-idx)))
                               (cons lst
                                     lsts)))))
        '())))

(export inclusive-range)
(define* (inclusive-range from to #:optional (step 1))
  (let ((n-between (- to from)))
    (iota (1+ (abs n-between)) from (* (sign n-between) step))))

(export fixed-point)
(define* (fixed-point proc init #:key (equal equal?) (max-steps 100) (diverged-thunk (const #f)))
  (let rec ((prev init)
            (cur (proc init))
            (steps-remaining max-steps))
    (cond ((equal prev cur)
           cur)
          ((zero? steps-remaining)
           (diverged-thunk))
          (else
           (rec cur (proc cur) (1- steps-remaining))))))

(define-public ((juxt . procs) . vals)
  (map (λ (proc) (apply proc vals)) procs))

(define-public (array-idx-set arr obj idx)
  (let ((ret (array-copy arr)))
    (apply array-set! ret obj idx)
    ret))

(define-public (make-hamt-from . init)
  (define (equal?-hash thing)
    (hash thing most-positive-fixnum))
  (let rec ((ret (make-hamt equal?-hash equal?))
            (to-insert (segment 2 init)))
    (if (null? to-insert) ret
        (rec (apply hamt-set ret (car to-insert))
             (cdr to-insert)))))

(define-public (alist->plist alist)
  (fold (match-lambda*
          (((key . val) acc)
           (cons* key val acc)))
        '()
        alist))

(export stream-tuples-in)
(define-stream (stream-tuples-in stream . streams)
  (define-stream (tuples-sum res item)
    (stream-append
     (stream-map (curry cons item)
                 (apply stream-tuples-in streams))
     res))
  (if (null? streams)
      (stream-map list stream)
      (stream-fold tuples-sum stream-null stream)))

(define-public ((arg n) . args)
  (list-ref args n))

(define*-public (assoc-update! alist key proc #:optional (dflt '()))
  (assoc-set! alist key
              (if-let (val (assoc-ref alist key))
                      (proc val)
                      (if (null? dflt)
                          (error "No such key ~a" key)
                          dflt))))

(define*-public (assoc-update alist key proc #:optional (dflt '()))
  (assoc-set alist key
             (if-let (val (assoc-ref alist key))
                     (proc val)
                     (if (null? dflt)
                         (error "No such key ~a" key)
                         dflt))))

(export memoized-lambda)
(define-syntax memoized-lambda
  (syntax-rules (memoized args val)
    ((_ arglst body ...)
     (let ((memoized (make-weak-key-hash-table)))
       (match-lambda*
         ((and args arglst)
          (if-let (val (hash-ref memoized args))
                  val
                  (let ((val (begin body ...)))
                    (hash-set! memoized args val)
                    val))))))))

(define-public (enqueue-all queue items)
  (fold (λ (item q) (enqueue q item))
        queue
        items))

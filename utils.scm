(define-module (utils)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (srfi srfi-171)
  #:use-module (srfi srfi-1)
  #:use-module (pipe))

(define-public (flatten lst)
  (list-transduce tflatten rcons lst))

(define-public (get-all-lines port)
  (string-split
   (get-string-all port)
   #\newline))

(define-public (sum lst)
  (apply + lst))

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
(define-syntax if-let
  (syntax-rules ()
    ((_ (var form)
        then
        else)
     (let ((var form))
       (if var
           then
           else)))))

(export when-let)
(define-syntax when-let
  (syntax-rules ()
    ((_ (var form)
        body ...)
     (let ((var form))
       (when var
         body ...)))))

(define-public (assoc-set alist key val)
  (alist-cons key val
              (remove (compose (curry equal? key)
                               car)
                      alist)))

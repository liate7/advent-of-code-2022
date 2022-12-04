(define-module (advent-of-code-2022 test)
  #:use-module (srfi srfi-64)
  #:export (with-test-suite in-test-suite)
  #:re-export (test-assert test-eqv test-equal test-eq
                           test-approximate test-error
                           test-read-eval-string))

(define* (with-test-suite suite-name thunk #:key count)
  (dynamic-wind (if count
                    (lambda () (test-begin suite-name count))
                    (lambda () (test-begin suite-name)))
                thunk
                (lambda () (test-end suite-name))))

(define-syntax in-test-suite
  (syntax-rules ()
    ((_ (suite-name args ...)
        body ...)
     (with-test-suite suite-name
                      (lambda ()
                        body ...)
                      args ...))))

;;; Local Variables:
;;; eval: (put 'in-test-suite 'scheme-indent-function 'defun)
;;; End:

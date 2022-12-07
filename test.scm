(define-module (test)
  #:use-module (srfi srfi-64)
  #:use-module (utils)
  #:export (with-test-suite in-test-suite run-module-tests)
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

(define (run-module-tests module)
  (when-let (tests-var (module-variable module 'tests))
      ((variable-ref tests-var))))

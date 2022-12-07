#!/usr/bin/env -S guile -e '(@ (run-tests) main)' -s
!#

(add-to-load-path (dirname (current-filename)))

(define-module (run-tests)
  #:use-module (ice-9 match)
  #:use-module (ice-9 ftw)
  #:use-module (utils)
  #:use-module (test)
  #:use-module (srfi srfi-1)
  #:use-module (pipe)
  #:export (run-all-tests main))

(define path (dirname (current-filename)))

;; Based heavily on `file-system-tree`s implementation
(define (get-module-names-under dir)
  (define (enter? name stat result)
	(not (equal? name ".git")))
  (define (leaf name stat result)
	(if (string-suffix? ".scm" name)
		(match result
		  (((siblings ...) rest ...)
		   (cons (cons (list (string->symbol
                              (string-drop-right (basename name) 4)))
					   siblings)
				 rest)))
		result))
  (define (down name stat result)
	(cons '() result))
  (define (up name stat result)
	(match result
	  (((children ...) (siblings ...) rest ...)
	   (cons (append (map (curry cons (string->symbol (basename name)))
						  children)
					 siblings)
			 rest))))
  (define (skip _name _stat result) result)
  (car (file-system-fold enter? leaf down up skip error '(()) dir)))

(define (run-all-tests)
  (->> (get-module-names-under path)
       (map cdr)
       (map resolve-module)
       (for-each run-module-tests)))

(define (main . args)
  (run-all-tests))

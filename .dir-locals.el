((scheme-mode . ((eval
				  put 'in-test-suite 'scheme-indent-function 'defun)
				 (eval
				  with-eval-after-load 'geiser-guile
				  (let ((root-dir (file-name-directory
								   (locate-dominating-file default-directory ".dir-locals.el"))))
					(require 'cl-lib)
					(cl-pushnew root-dir geiser-guile-load-path :test #'string-equal))))))

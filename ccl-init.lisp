(load #P"~/quicklisp/setup.lisp")

(defun module-provide-quicklisp (&rest args)
  (ignore-errors (apply 'ql:quickload args)))

(setf ccl::*module-provider-functions*
      '(CCL::MODULE-PROVIDE-SEARCH-PATH
	module-provide-quicklisp
	ASDF::MODULE-PROVIDE-ASDF))

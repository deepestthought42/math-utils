;;;; math-utils.asd

(asdf:defsystem #:math-utils
  :serial t
  :description "Small lisp library containing math utilities for my data analysis packages."
  :author "Renee Klawitter <klawitterrenee@gmail.com>"
  :license "Apache 2.0"
  :depends-on (#:iterate
               #:alexandria)
  :components ((:file "package")
	       (:file "moments")
               (:file "math-utils")))


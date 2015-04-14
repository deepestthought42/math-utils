;;;; math-utils.asd

(asdf:defsystem #:math-utils
  :serial t
  :description "Describe math-utils here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:iterate
               #:alexandria)
  :components ((:file "package")
	       (:file "partition")
	       (:file "fit")
               (:file "math-utils")))


;;;; package.lisp

(defpackage #:math-utils
  (:use #:cl #:iterate #:let-plus)
  (:export
   #:online-variance
   #:moment
   #:mean
   #:variance
   #:std-deviation
   #:std-error))





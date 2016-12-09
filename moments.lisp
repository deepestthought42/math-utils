(in-package #:math-utils)


;;; mean and variance

(define-condition insufficient-data (arithmetic-error) ())


(defclass moment ()
  ((mean :accessor mean :initarg :mean :initform 0)
   (variance :accessor variance :initarg :variance :initform 0)
   (std-deviation :accessor std-deviation :initarg :std-deviation :initform 0)
   (std-error :accessor std-error :initarg :std-error :initform 0)
   (no-of-data :accessor no-of-data :initarg :no-of-data :initform 0)))

(defun online-variance (data &key (error-on-n=1 t) 
				  (error-on-n=0 t)
				  (default-mean 0d0)
				  (default-std-deviation 0d0))
  "Calculates the mean, variance, and std. deviation and std. error of sequence DATA with equal
  weights as described in:
  https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Online_algorithm

=> MOMENT
"
  (cond
    ((= (length data) 0)
     (if error-on-n=0
	 (error 'insufficient-data)
	 (return-from online-variance
	   (make-instance 'moment
			  :mean default-mean
			  :variance (* default-std-deviation
				       default-std-deviation)
			  :std-deviation default-std-deviation
			  :std-error default-std-deviation
			  :no-of-data 0))))
    ((= (length data) 1)
     (if error-on-n=1
	 (error 'insufficient-data)
	 (return-from online-variance
	   (make-instance 'moment
			  :mean (elt data 0)
			  :variance (* default-std-deviation
				       default-std-deviation)
			  :std-deviation default-std-deviation
			  :std-error default-std-deviation
			  :no-of-data 1)))))
  (iter
    (with mean = 0)
    (with M2 = 0)
    (for x in-sequence data)
    (for n initially 1 then (1+ n))
    (for delta = (- x mean))
    (incf mean (/ delta n))
    (incf M2 (* delta (- x mean)))
    (finally
     (let* ((variance (/ m2 (1- n)))
	    (std-dev (sqrt variance))
	    (std-error (sqrt (/ variance n))))
       (return (make-instance 'moment
			      :mean mean
			      :variance variance
			      :std-deviation std-dev
			      :std-error std-error
			      :no-of-data n))))))


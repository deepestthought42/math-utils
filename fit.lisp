;;;; math-utils.lisp

(in-package #:math-utils)

;;; "math-utils" goes here. Hacks and glory await!


;;Convert a number to a double-float
(defun dfloat (x)
  (coerce x 'double-float))

(defun gser (a x &key (itmax 100) (eps 3.0d-7))
 (declare (type double-float a x eps))
 (declare (type fixnum itmax))

 (prog ((gamser 0d0) (gln 0d0) (ap 0d0) 
        (del 0d0) (sum 0d0))
  (declare (type double-float gamser gln ap del sum))


  (setf gln (gammln a)) 
  (when 
   (<= x 0)  
   (if (< x 0d0) (error " invalid argument to gser "))
   (setf gamser 0d0) 
   (return (values gamser gln))) 

  (setf ap a) 
  (setf sum (/ 1d0 a)) 
  (setf del sum) 
  (do ((n 1 (+ n 1)))
      ((> n itmax) t)
      (declare (type fixnum n))
    (setf ap (+ ap 1))
    (setf del (/ (* del x) ap))
    (setf sum (+ sum del))
    (if (< (abs del) (* (abs sum) eps)) (go label1))) 

  (error " a too large , itmax too small in gser ") 
  label1 
  (setf gamser (* sum
                  (exp (+ (- x) (* a (log x)) (- gln))))) 
   
  (return (values gamser gln))))

(defun gammln (xx)
  (declare (type double-float xx)) 

  (prog ((cof (make-array 6 :element-type 'double-float :initial-contents
	       '(76.18009173d0 -86.50532033d0 24.01409822d0 
		 -1.231739516d0 0.120858003d-2 -0.536382d-5)))
	 (stp 0d0) (half 0d0) (one 0d0) (fpf 0d0) 
	 (tmp 0d0) (ser 0d0) (gammln 0d0) (x 0d0))

     (declare (type (simple-array double-float (*)) cof)) 
     (declare (type double-float stp half one fpf tmp ser x gammln)) 

     (setq stp  2.50662827465d0)

     (setq half 0.5d0 one 1.0d0 fpf 5.5d0) 
 
     (setf x (1- xx)) 
     (setf tmp (+ x fpf)) 
     (setf tmp (- (* (+ x half) (log tmp)) tmp)) 
     (setf ser one) 
     (do ((j 0 (+ j 1)))
	 ((> j 5) t)
       (declare (type fixnum j))
       (setf x (1+ x))
       (setf ser (+ ser (/ (aref cof j) x)))) 

     (setf gammln (+ tmp (log (* stp ser)))) 
     (return (the double-float gammln))))


;----------------------------------------------------------------------------

(defun gcf (a x &key (itmax 100) (eps 3.0d-7))
 (declare (type double-float a x eps))
 (declare (type fixnum itmax))

 (prog ((gammcf 0d0) (gln 0d0) (gold 0d0) (a0 0d0) (a1 0d0) 
        (b0 0d0) (b1 0d0) (fac 0d0) (an 0d0) (ana 0d0) (anf 0d0) (g 0d0))
  (declare (type double-float gln gammcf gold a0 a0 b0 b1 fac an ana anf g))

  (setf gln (gammln a)) 
  (setf gold 0d0) 
  (setf a0 1d0) 
  (setf a1 x) 
  (setf b0 0d0) 
  (setf b1 1d0) 
  (setf fac 1d0) 
  (do ((n 1 (+ n 1)))
      ((> n itmax) t)
    (setf an (dfloat n))
    (setf ana (- an a))
    (setf a0 (* (+ a1 (* a0 ana)) fac))
    (setf b0 (* (+ b1 (* b0 ana)) fac))
    (setf anf (* an fac))
    (setf a1 (+ (* x a0) (* anf a1)))
    (setf b1 (+ (* x b0) (* anf b1)))
    (when 
     (not (= a1 0d0))
     (setf fac (/ 1d0 a1))
     (setf g (* b1 fac)) 
     (if (< (abs (/ (- g gold) g)) eps) (go label1))
     (setf gold g))) 
   
  (error " a too large , itmax too small in gcf ") 
  label1 
  (setf gammcf (* (exp (+ (- x) (* a (log x)) (- gln)))
                  g)) 
   
  (return (values gammcf gln))))

(defun gammq (a x)
 (declare (type double-float a x))

 (prog ((gamser 0d0) (gammcf 0d0) (gammq 0d0))
  (declare (type double-float gamser gammcf gammq))

  (if (or (< x 0d0) (<= a 0d0)) (error " invalid arguments for gammq")) 
  (cond 
    ((< x (+ a 1d0))
     (setq gamser (gser a x)) 
     (setf gammq (- 1d0 gamser))) 
    (t
     (setq gammcf (gcf a x)) 
     (setf gammq gammcf))) 
 
  (return (the double-float gammq))))

(defun fit (x y sig mwt)
 (declare (type (simple-array double-float (*)) x)) 
 (declare (type (simple-array double-float (*)) y)) 
 (declare (type (simple-array double-float (*)) sig)) 
 (declare (type fixnum mwt))
 (prog ((a 0d0) (b 0d0) (siga 0d0) (sigb 0d0) (chi2 0d0) (q 0d0)
        (sx 0d0) (sy 0d0) (st2 0d0) (ndata 0) (t0 0d0)
        (ss 0d0) (wt 0d0) (sxoss 0d0) (sigdat 0d0))
  (declare (type double-float a b siga sigb chi2 q sx sy 
                              st2 t0 ss wt sxoss sigdat))
  (declare (type fixnum ndata))

  (setq ndata (array-dimension x 0))
  (setf sx 0d0) 
  (setf sy 0d0) 
  (setf st2 0d0) 
  (setf b 0d0) 

  (cond 
   ((not (= mwt 0))  
    (setf ss 0d0)
    (do ((i 0 (+ i 1)))
        ((> i (1- ndata)) t)
        (declare (type fixnum i))
      (setf wt (/ 1d0 (expt (aref sig i) 2)))
      (setf ss (+ ss wt))
      (setf sx (+ sx (* (aref x i) wt)))
      (setf sy (+ sy (* (aref y i) wt)))))
   (t
    (do ((i 0 (+ i 1)))
        ((> i (1- ndata)) t)
        (declare (type fixnum i))
      (setf sx (+ sx (aref x i)))
      (setf sy (+ sy (aref y i))))
    (setf ss (dfloat ndata))) )

  (setf sxoss (/ sx ss)) 

  (cond 
   ((not (= mwt 0))
    (do ((i 0 (+ i 1)))
        ((> i (1- ndata)) t)
      (setf t0 (/ (- (aref x i) sxoss) (aref sig i)))
      (setf st2 (+ st2 (* t0 t0)))
      (setf b (+ b (/ (* t0 (aref y i)) (aref sig i))))))
   (t
    (do ((i 0 (+ i 1)))
        ((> i (1- ndata)) t)
        (declare (type fixnum i))
      (setf t0 (- (aref x i) sxoss))
      (setf st2 (+ st2 (* t0 t0)))
      (setf b (+ b (* t0 (aref y i)))))))

  (setf b (/ b st2)) 
  (setf a (/ (- sy (* sx b)) ss)) 
  (setf siga (sqrt (/ (1+ (/ (* sx sx) (* ss st2))) ss))) 
  (setf sigb (sqrt (/ 1d0 st2))) 
  (setf chi2 0d0) 

  (cond 
   ((= mwt 0)
    (do ((i 0 (+ i 1)))
        ((> i (1- ndata)) t)
        (declare (type fixnum i))
      (setf chi2 (+ chi2
                   (expt (- (- (aref y i) a) (* b (aref x i)))
                         2))))
   (setf q 1d0) 
   (setf sigdat (sqrt (/ chi2 (dfloat (- ndata 2)))))
   (setf siga (* siga sigdat)) (setf sigb (* sigb sigdat))) 
   (t
    (do ((i 0 (+ i 1)))
        ((> i (1- ndata)) t)
         (declare (type fixnum i))
     (setf chi2 (+ chi2
                   (expt (/ (- (- (aref y i) a) (* b (aref x i)))
                            (aref sig i))
                         2))))
   (setf q (gammq (* 0.5d0 (- ndata 2)) (* 0.5d0 chi2)))))

  (return (values a b siga sigb chi2 q))))

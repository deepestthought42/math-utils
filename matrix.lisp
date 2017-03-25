(in-package #:math-utils)


(defun invert-matrix (in-matrix)
  "Given a square matrix in IN-MATRIX returns 
INVERSE-OF-IN-MATRIX, determinant"
  (let* ((out-matrix (make-array (array-dimensions in-matrix)))
	 (dim (array-dimension in-matrix 0)) ;; dimension of matrix
	 (det 1)                               ;; determinant of matrix
	 (l nil)                               ;; permutation vector
	 (m nil)                               ;; permutation vector
	 (temp 0))       

    (if (not (equal dim (array-dimension in-matrix 1)))
	(error "invert-matrix () - matrix not square"))

    (if (not (equal (array-dimensions in-matrix) 
		    (array-dimensions out-matrix)))
	(error "invert-matrix () - matrices not of the same size"))

    ;; copy in-matrix to out-matrix if they are not the same
    (when (not (equal in-matrix out-matrix))
      (do ((i 0 (1+ i)))
	  ((>= i dim))    
	(do ((j 0 (1+ j)))
	    ((>= j dim)) 
	  (setf (aref out-matrix i j) (aref in-matrix i j)))))

    ;; allocate permutation vectors for l and m, with the 
    ;; same origin as the matrix
    (setf l (make-array `(,dim)))
    (setf m (make-array `(,dim)))

    (do ((k 0 (1+ k))
	 (biga 0)
	 (recip-biga 0))
	((>= k dim))

      (setf (aref l k) k)
      (setf (aref m k) k)
      (setf biga (aref out-matrix k k))

      ;; find the biggest element in the submatrix
      (do ((i k (1+ i)))
	  ((>= i dim))    
	(do ((j k (1+ j)))
	    ((>= j dim)) 
	  (when (> (abs (aref out-matrix i j)) (abs biga))
	    (setf biga (aref out-matrix i j))
	    (setf (aref l k) i)
	    (setf (aref m k) j))))

      ;; interchange rows
      (if (> (aref l k) k)
	  (do ((j 0 (1+ j))
	       (i (aref l k)))
	      ((>= j dim)) 
	    (setf temp (- (aref out-matrix k j)))
	    (setf (aref out-matrix k j) (aref out-matrix i j))
	    (setf (aref out-matrix i j) temp)))

      ;; interchange columns 
      (if (> (aref m k) k)
	  (do ((i 0 (1+ i))
	       (j (aref m k)))
	      ((>= i dim)) 
	    (setf temp (- (aref out-matrix i k)))
	    (setf (aref out-matrix i k) (aref out-matrix i j))
	    (setf (aref out-matrix i j) temp)))

      ;; divide column by minus pivot (value of pivot 
      ;; element is in biga)
      (if (equalp biga 0) 
	  (return-from invert-matrix 0))
      (setf recip-biga (/ 1 biga))
      (do ((i 0 (1+ i)))
	  ((>= i dim)) 
	(if (not (equal i k))
	    (setf (aref out-matrix i k) 
		  (* (aref out-matrix i k) (- recip-biga)))))

      ;; reduce matrix
      (do ((i 0 (1+ i)))
	  ((>= i dim)) 
	(when (not (equal i k))
	  (setf temp (aref out-matrix i k))
	  (do ((j 0 (1+ j)))
	      ((>= j dim)) 
	    (if (not (equal j k))
		(incf (aref out-matrix i j) 
		    (* temp (aref out-matrix k j)))))))

      ;; divide row by pivot
      (do ((j 0 (1+ j)))
	  ((>= j dim)) 
	(if (not (equal j k))
	    (setf (aref out-matrix k j)
		  (* (aref out-matrix k j) recip-biga))))

      (setf det (* det biga))       ;; product of pivots
      (setf (aref out-matrix k k) recip-biga)) ;; k loop

    ;; final row & column interchanges
    (do ((k (1- dim) (1- k)))
	((< k 0))
      (if (> (aref l k) k)
	  (do ((j 0 (1+ j))
	       (i (aref l k)))
	      ((>= j dim))
	    (setf temp (aref out-matrix j k))
	    (setf (aref out-matrix j k) 
		  (- (aref out-matrix j i)))
	    (setf (aref out-matrix j i) temp)))
      (if (> (aref m k) k)
	  (do ((i 0 (1+ i))
	       (j (aref m k)))
	      ((>= i dim))
	    (setf temp (aref out-matrix k i))
	    (setf (aref out-matrix k i) 
		  (- (aref out-matrix j i)))
	    (setf (aref out-matrix j i) temp))))
    (values out-matrix det)))

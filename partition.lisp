(in-package #:math-utils)

(declaim (optimize (debug 3) (speed 0)))



;; partition(int s[], int n, int k)
;; {
;; int m[MAXN+1][MAXK+1];
;; int d[MAXN+1][MAXK+1];
;; int p[MAXN+1];
;; int cost;
;; int i,j,x;
;; /*
;; /*
;; /*
;; /*
;; /*
;; DP table for values */
;; DP table for dividers */
;; prefix sums array */
;; test split cost */
;; counters */
;; p[0] = 0;
;; for (i=1; i<=n; i++) p[i]=p[i-1]+s[i]; /* construct prefix sums */
;; for (i=1; i<=n; i++) m[i][1] = p[i];
;; for (j=1; j<=k; j++) m[1][j] = s[1]; /* initialize boundaries */
;; for (i=2; i<=n; i++)
;; /* evaluate main recurrence */
;; for (j=2; j<=k; j++) {
;; m[i][j] = MAXINT;
;; for (x=1; x<=(i-1); x++) {
;; cost = max(m[x][j-1], p[i]-p[x]);
;; if (m[i][j] > cost) {
;; m[i][j] = cost;
;; d[i][j] = x;
;; }
;; }
;; }
;; reconstruct_partition(s,d,n,k);
;; /* print book partition */
;; }


;; reconstruct_partition(int s[],int d[MAXN+1][MAXK+1], int n, int k)
;; {
;; if (k==1)
;; print_books(s,1,n);
;; else {
;; reconstruct_partition(s,d,d[n][k],k-1);
;; print_books(s,d[n][k]+1,n);
;; }
;; }
;; print_books(int s[], int start, int end)
;; {
;; int i;
;; /* counter */
;; for (i=start; i<=end; i++) printf(" %d ",s[i]);
;; printf("\n");
;; }

(defparameter *max-int* 100000000)


(defun add-to-ret (retval s start end)
  (let ((to-ret))
    (iter (for i from (- start 1) below end)
      (push (elt s i) to-ret))
    (if to-ret
	(vector-push-extend (reverse to-ret) retval))))

(defun reconstruct-partition (retval s d n k)
  (if (equal 1 k)
      (add-to-ret retval s 1 n)
      (progn
	(reconstruct-partition retval s d (aref d n k) (- k 1))
	(add-to-ret retval s (+ (aref d n k) 1) n))))

(defun partition (s k &key (key #'identity))
  (let* ((n (length s))
	 (m (make-array (list (1+ n) (1+ k)) :initial-element 0))
	 (d (make-array (list (1+ n) (1+ k)) :initial-element 0))
	 (p (make-array (list (1+ n)) :initial-element 0))
	 (cost 0))
    (setf (aref p 0) 0)
    (iter (for i from 1 to n)
      (setf (aref p i) (+ (aref p (- i 1))
			  (funcall key (elt s (- i 1)))))
      (setf (aref m i 1) (aref p i)))
    (iter (for j from 1 to k)p
      (setf (aref m 1 j) (funcall key (elt s 1))))

    (iter (for i from 2 to n)
      (iter (for j from 2 to k)
	(setf (aref m i j) *max-int*)
	(iter (for x from 1 below i)
	  (setf cost (max (aref m x (- j 1))
			  (- (aref p i)
			     (aref p x))))
	  (if (> (aref m i j) cost)
	      (progn
		(setf (aref m i j) cost
		      (aref d i j) x))))))
    (let ((ret-val (make-array '(0) :fill-pointer 0 :adjustable t)))
      (reconstruct-partition ret-val s d n k)
      (if (not (equal (length ret-val) k))
	  (warn "Could not partition given array into wanted (~a) number of count classes!" k))
      ret-val)))









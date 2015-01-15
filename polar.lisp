(in-package :roids)

(defclass polar ()
  ((r :initarg :r :initform 0 :reader r)
   (theta :initarg :theta :initform 0 :reader theta)))

(defun polar (&optional (r 0) (theta 0))
  (make-instance 'polar :r r :theta theta))

(defmethod add ((p1 polar) (p2 polar))
  (from-pt (map 'vector #'+ (to-pt p1) (to-pt p2))))

(defmethod sub ((p1 polar) (p2 polar))
  (from-pt (map 'vector #'- (to-pt p1) (to-pt p2))))

(defmethod to-pt ((p polar))
  (with-slots (r theta) p
    (vector (* r (cos theta))
	    (* r (sin theta)))))

(defmethod from-pt ((v vector))
  (let ((x (aref v 0))
	(y (aref v 1)))
    (polar (sqrt (+ (expt x 2) (expt y 2)))
	   (atan y x))))

(defun offset-pt (pt dx dy &optional (facing 0))
  (with-slots (r theta) (from-pt (vector dx dy))
    (to-pt (add (from-pt pt) (polar r (+ theta facing))))))

(defmethod print-object ((p polar) stream)
  (with-slots (r theta) p
    (format stream "(polar (r ~$) (theta ~$))" r theta)))

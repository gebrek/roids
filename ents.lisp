;;;; ents.lisp

(in-package #:roids)

(defclass ship ()
  ((pos :initform #(0 0) :initarg :pos :accessor pos)
   (fac :initform 0 :initarg :fac :accessor fac)
   (vel :initform (polar) :initarg :vel :accessor vel)))

(defmethod draw ((ship ship))
  (with-slots (pos fac) ship
    (let* ((length 20)
	   (width  15)
	   (front
	    (offset-pt pos (* length 1/2) 0 fac))
	   (back
	    pos)
	   (left
	    (offset-pt front (- length) (* width 1/2) fac))
	   (right
	    (offset-pt front (- length) (* width -1/2) fac)))
      (with-round-vectors (front back left right)
	(sdl:draw-line left front)
	(sdl:draw-line front right)
	(sdl:draw-bezier (list left back right))))))

(defmethod move ((ship ship))
  (with-slots (pos vel) ship
    (let ((offset (to-pt vel)))
      (setf pos (offset-pt pos (aref offset 0) (aref offset 1))))))

(defmethod accel ((ship ship))
  (with-slots (vel fac) ship
    (let ((new-v (add vel (polar 0.3 fac)))
	  (speed-limit 7))
      (if (< speed-limit (r new-v))
	  (setf vel (polar speed-limit (theta new-v)))
	  (setf vel new-v)))))

(defmethod wrap ((ship ship) width height)
  (with-slots (pos) ship
    (let ((x (aref pos 0))
	  (y (aref pos 1)))
      (cond ((< x 0) (setf (aref pos 0) width))
	    ((> x width) (setf (aref pos 0) 0)))
      (cond ((< y 0) (setf (aref pos 1) height))
	    ((> y height) (setf (aref pos 1) 0))))))

(defmacro with-round-vectors (nums &body body)
  `(let
       ,(mapcar (lambda (entry)
		  (let ((use-name
			 (if (symbolp entry)
			     entry
			     (car entry)))
			(var-name
			 (if (symbolp entry)
			     entry
			     (cadr entry))))
		    `(,use-name
		      (map 'vector #'round ,var-name))))
		nums)
     ,@body))

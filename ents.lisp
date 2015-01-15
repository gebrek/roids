;;;; ents.lisp

(in-package #:roids)

;;; Entity abstract

(defvar *world* '())

(defclass entity ()
  ((pos :initform #(0 0) :initarg :pos :accessor pos)
   (fac :initform 0 :initarg :fac :accessor fac)
   (vel :initform (polar) :initarg :vel :accessor vel)))

(defmethod initialize-instance :after ((entity entity) &rest initargs)
  (declare (ignorable initargs))
  (push entity *world*))

;;; Generics

(defgeneric wrap (entity width height))
(defgeneric draw (entity))
(defgeneric move (entity))
(defgeneric accel (entity))
(defgeneric shoot (entity))
(defgeneric destroy (entity))
(defgeneric collide (entity entity))

(defmethod collide ((ent0 entity) (ent1 entity))
  ;; do nothing, but an isomorph should be there hopefully
  '())

(defmethod wrap ((entity entity) width height)
  (with-slots (pos) entity
    (let ((x (aref pos 0))
	  (y (aref pos 1)))
      (cond ((< x 0) (setf (aref pos 0) width))
	    ((> x width) (setf (aref pos 0) 0)))
      (cond ((< y 0) (setf (aref pos 1) height))
	    ((> y height) (setf (aref pos 1) 0))))))

(defmethod move ((entity entity))
  (with-slots (pos vel) entity
    (let ((offset (to-pt vel)))
      (setf pos (offset-pt pos offset)))))

;;; Ship definition

(defclass ship (entity)
  ((radius :initform 7.5 :initarg :radius :accessor radius)))

(defmethod draw ((ship ship))
  (with-slots (pos fac) ship
    (let* ((length 20)
	   (width  15)
	   (front
	    (offset-pt-* pos (* length 1/2) 0 fac))
	   (back
	    pos)
	   (left
	    (offset-pt-* front (- length) (* width 1/2) fac))
	   (right
	    (offset-pt-* front (- length) (* width -1/2) fac)))
      (with-round-vectors (front back left right)
	(sdl:draw-line left front)
	(sdl:draw-line front right)
	(sdl:draw-bezier (list left back right))))))

;; (defmethod move ((ship ship))
;;   (with-slots (pos vel) ship
;;     (let ((offset (to-pt vel)))
;;       (setf pos (offset-pt pos offset)))))

(defmethod accel ((ship ship))
  (with-slots (vel fac) ship
    (let ((new-v (add vel (polar 0.3 fac)))
	  (speed-limit 7))
      (if (< speed-limit (r new-v))
	  (setf vel (polar speed-limit (theta new-v)))
	  (setf vel new-v)))))

(defmethod shoot ((ship ship))
  (with-slots (fac pos) ship
    (make-instance 'bullet :vel (polar 9 fac) :pos pos)))

(defmethod destroy ((ship ship))
  ())

;;; Bullet class

(defclass bullet (entity)
  ((quote :initform nil)))

(defmethod draw ((bullet bullet))
  (sdl:draw-circle (round-vector (pos bullet)) 1))

;; (defmethod move ((bullet bullet))
;;   (with-slots (pos vel) bullet
;;     (let ((offset (to-pt vel)))
;;       (setf pos (offset-pt pos offset)))))

(defmethod wrap ((bullet bullet) width height)
  (with-slots (pos) bullet
    (let ((x (aref pos 0))
	  (y (aref pos 1)))
      (if (or (< x 0) (> x width)
	      (< y 0) (> y height))
	  (destroy bullet)))))

(defmethod destroy ((bullet bullet))
  (setf *world* (remove bullet *world*)))

;;; Asteroid

(defclass asteroid (entity)
  ((radius :initform 20 :initarg :radius :accessor radius)
   (rel-verts :initform '() :accessor rel-verts)))

(defmethod initialize-instance :after ((asteroid asteroid) &rest initargs)
  (declare (ignore initargs))
  (with-slots (radius rel-verts vel) asteroid
    (setf rel-verts (gen-asteroid-verts radius))
    (setf vel (polar (/ 30 radius) (* pi (/ (random 8) 4))))))

(defun gen-asteroid-verts (radius)
  (let ((points 8))
    (loop :for i :from 0 :upto (1- points)
       :collect (polar (+/- radius
			    (random (round (half radius))))
		       (* pi (/ i (half points))))
       :into verts
       :finally (return (append (last verts) verts)))))

(defmethod draw ((asteroid asteroid))
  (with-slots (pos rel-verts) asteroid
    (let ((abs-verts (mapcar (lambda (w)
			       (round-vector (offset-pt pos (to-pt w))))
			     rel-verts)))
      (maplist (lambda (x) (when (> (length x) 1)
			     (sdl:draw-line (car x) (cadr x))))
	       abs-verts))))

(defmethod destroy ((asteroid asteroid))
  (with-slots (radius pos) asteroid
    (cond ((> radius 5)
	   (dotimes (i 2)
	     (make-instance 'asteroid
			    :radius (- radius 5)
			    :pos pos)))
	  (t
	   '()))))

;;; Collisions methods
;;; because of how do-collisions is written isomorphic methods
;;; aren't needed. i think.

(defmethod collide ((asteroid asteroid) (ship ship))
  (when (< (sdl:distance (pos asteroid) (pos ship))
	   (+ (radius ship) (radius asteroid)))
    (destroy ship)))

(defmethod collide ((asteroid asteroid) (bullet bullet))
  (when (< (sdl:distance (pos asteroid) (pos bullet))
	   (radius asteroid))
    (destroy bullet)
    (destroy asteroid)))

(make-instance 'asteroid :pos (vector (random *width*) (random *height*)))
(setf *world* '())
;;; 

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

(defun round-vector (v)
  (map 'vector #'round v))

(defun +/- (&rest numbers)
  (if (= 1 (random 2))
      (apply #'+ numbers)
      (apply #'- numbers)))

(defun half (n)
  (/ n 2))

(defun do-collisions ()
  (mapcar (lambda (x)
	    (mapcar (lambda (y)
		      (collide x y))
		    (remove x *world*)))
	  *world*))

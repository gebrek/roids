;;;; ents.lisp

(in-package #:roids)

(defclass ship ()
  ((pos :initform #(0 0) :initarg :pos)
   (fac :initform 0 :initarg :fac)))

(defmethod draw-ship ((ship ship))
  (with-slots (pos fac) ship
    (let* ((length 20)
	   (width  15)
	   (front
	    (offset-pt pos (* length 1/2) 0 fac))
	   (back
	    (offset-pt pos (* length -3/8) 0 fac))
	   (left
	    (offset-pt front length (* width 1/2) fac))
	   (right
	    (offset-pt front length (* width -1/2) fac)))
      (sdl:draw-line left front)
      (sdl:draw-line front right)
      (sdl:draw-curve (list right back left)))))


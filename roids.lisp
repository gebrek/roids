;;;; roids.lisp

(in-package #:roids)

;;; "roids" goes here. Hacks and glory await!

(defvar *width* 640)
(defvar *height* 480)

(defvar *player*
  (make-instance 'ship
		 :pos (vector (/ *width* 2) (/ *height* 2))))

(defvar *lives* 3)

(defun start-game ()
  (setf *world* nil)
  (setf *lives* 3)
  (setf *player* (make-instance 'ship
				:pos (vector (half *width*) (half *height*))))
  (dotimes (i 8)
    (make-instance 'asteroid :pos (vector (/ i 7) 0)))
  (game-loop))

(defun game-loop ()
  (sdl:with-init ()
    (sdl:window *width* *height*)
    (sdl:enable-key-repeat 1 250)
    (sdl:with-events ()
      (:quit-event () t)
      (:key-down-event (:key key)
		       (key-down key))
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (events)
	     (sdl:update-display)))))

(defun events ()
  ;(draw *player*)
  (when (sdl:key-held-p :sdl-key-d)
    (incf (fac *player*) 0.05))
  (when (sdl:key-held-p :sdl-key-a)
    (decf (fac *player*) 0.05))
  (when (sdl:key-held-p :sdl-key-w)
    (accel *player*))

  (mapcar (lambda (x) (wrap x *width* *height*)) *world*)
  (mapcar #'move *world*)
  (do-collisions)
  (mapcar #'draw *world*))

(defun key-down (key)
  (case key
    (:sdl-key-s (shoot *player*))))


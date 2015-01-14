;;;; roids.lisp

(in-package #:roids)

;;; "roids" goes here. Hacks and glory await!

(defvar *width* 320)
(defvar *height* 240)

(defvar *player*
  (make-instance 'ship
		 :pos (vector (/ *width* 2) (/ *height* 2))))

(defun game-loop ()
  (sdl:with-init ()
    (sdl:window 320 240)
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ()
	     (sdl:clear-display sdl:*black*)
	     (draw-ship *player*)
	     (sdl:update-display)
	     )
      )
    ))

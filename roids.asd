;;;; roids.asd

(asdf:defsystem #:roids
  :description "asteroids clone in cl"
  :author "jacob sonnenberg"
  :license ""
  :depends-on (#:lispbuilder-sdl)
  :serial t
  :components ((:file "package")
               (:file "roids")))


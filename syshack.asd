;;;; syshack.asd

(asdf:defsystem #:syshack
  :description "Describe syshack here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:sdl2-image #:sdl2-mixer #:swank #:imago #:cl-ppcre #:cl-tiled)
  :components ((:file "package")
               (:file "syshack")
	       (:file "asset_manager")
	       (:file "entity")
	       (:file "animation")
	       (:file "component")
	       (:file "camera")
	       (:file "texture")
	       (:file "hangul")
	       (:file "bitmap_font")
	       (:file "util")
	       (:file "tiled")
	       (:file "scene")
	       (:file "scene_zelda")
	       (:file "game")))

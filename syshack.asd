;;;; syshack.asd

(asdf:defsystem #:syshack
  :description "Top view RPG themed system hacking"
  :author "Yeonho Jang<siabard@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2 #:sdl2-image #:sdl2-mixer #:swank #:imago #:cl-ppcre #:cl-tiled #:arrows )
  :components ((:file "package")
               (:file "syshack")
	       (:module "src"
		:components ((:file "action")
			     (:file "color")
			     (:file "asset_manager")
			     (:file "sprite")
			     (:file "physics")
			     (:file "key")
			     (:file "entity")
			     (:file "animation")
			     (:file "component")
			     (:file "camera")
			     (:file "texture")
			     (:file "hangul")
			     (:file "bitmap_font")
			     (:file "util")
			     (:file "gamemap")
			     (:file "tiled")
			     (:file "text")
			     (:file "global")
			     (:file "dialog_loader")
			     (:file "dialog")
			     (:file "scene")
			     (:file "scene_zelda")
			     (:file "game")))
	       (:module "gui"
		:components ((:file "gui")
			     (:file "panel")
			     (:file "dialog-window")))))

(in-package #:syshack)

(defclass <caction> ()
  ((name :accessor caction-name
	 :initarg :name)
   (act :accessor caction-act
	:initarg :act
	:documentation "'move-to")
   (pos :accessor caction-pos
	:initarg :pos
	:initform nil)))

(defun make-caction (name act &key pos)
  (make-instance '<caction>
		 :name name
		 :act act
		 :pos pos))

;; (make-action "moveE" 'move-to :pos (make-vec2 70 60))

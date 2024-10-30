(in-package #:syshack)

(defclass <caction> ()
  ((name :accessor caction-name
	 :initarg :name)
   (act :accessor caction-act
	:initarg :act
	:documentation "'move-to")
   (pos :accessor caction-pos
	:initarg :pos
	:initform nil)
   (size :accessor caction-size
	 :initarg :size
	 :initform nil)
   (msg :accessor caction-msg
	:initarg :msg
	:initform nil)))

(defun make-caction (name act &key pos)
  (make-instance '<caction>
		 :name name
		 :act act
		 :pos pos))

;; (make-action "moveE" 'move-to :pos (make-vec2 70 60))
;; (make-action "show-panel" 'show-panel :pos (make-vec2 120 80) :size (make-vec2 320 200) :msg "TEXT 안녕하세요")

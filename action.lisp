(in-package #:syshack)

(defclass <caction> ()
  ((name :accessor caction-name
	 :initarg :name)
   (act :accessor caction-act
	:initarg :act)
   (pos :accessor caction-pos
	:initarg :pos
	:initform nil)))

(defun make-caction (name act &optional (pos nil))
  (make-instance '<caction>
		 :name name
		 :act act
		 :pos pos))

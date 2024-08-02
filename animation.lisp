(in-package #:syshack)

(defclass <animation> ()
  ((name :accessor animation-name
	 :initarg :name
	 :type string)
   (atlas-name :accessor animation-atlas-name
	       :initarg :atlas-name
	       :type string)
   (frames :accessor animation-frames
	   :initarg :frames
	   :initform '())
   (total-frame :accessor animation-total-frame
		:initarg :total-frame)))
		

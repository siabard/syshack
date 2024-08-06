(in-package #:syshack)

(defclass <animation> ()
  ((name :accessor animation-name
	 :initarg :name
	 :type string)
   (texture-name :accessor animation-texture-name
		 :initarg :texture-name
		 :type string)
   (frames :accessor animation-frames
	   :initarg :frames
	   :initform '())
   (total-frame :accessor animation-total-frame
		:initarg :total-frame)))
		

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
   (frame-length :accessor animation-frame-length
		 :initarg :frame-length)))
		
(defun make-animation (name texture-name start-frame frame-length)
  (let* ((last-frame (- (+ start-frame frame-length) 1))
	 (frames (loop for i from start-frame below last-frame collect i)))
  (make-instance '<animation>
		 :name name
		 :texture-name texture-name
		 :frames frames
		 :frame-length frame-length)))

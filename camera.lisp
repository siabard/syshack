(in-package #:syshack)

(defclass <camera> ()
  ((name :accessor camera-name
	 :initarg :name
	 :type string)
   (x :initarg :x
      :accessor camera-x)
   (y :initarg :y
      :accessor camera-y)
   (w :initarg :w
      :accessor camera-w)
   (h :initarg :h
      :accessor camera-h)))


(defun make-camera (name x y w h)
  (make-instance '<camera>
		 :name name
		 :x x
		 :y y
		 :w w
		 :h h))

(defgeneric get-rect (camera)
  (:documentation "return sdl2 rect"))


(defmethod get-rect ((camera <camera>))
  (sdl2:make-rect (camera-x camera)
		  (camera-y camera)
		  (camera-w camera)
		  (camera-h camera)))

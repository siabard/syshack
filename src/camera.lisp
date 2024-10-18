(in-package #:syshack)

(defclass <camera> ()
  ((name :accessor camera-name
	 :initarg :name
	 :type string)
   (x :initarg :x
      :accessor camera-x)
   (y :initarg :y
      :accessor camera-y)
   (target-x :initarg :target-x
	     :accessor camera-target-x
	     :initform 0)
   (target-y :initarg :target-y
	     :accessor camera-target-y
	     :initform 0)
   (max-x :initarg :max-x
	  :accessor camera-max-x
	  :initform 0)
   (max-y :initarg :max-y
	  :accessor camera-max-y
	  :initform 0)
   (w :initarg :w
      :accessor camera-w)
   (h :initarg :h
      :accessor camera-h)))


(defun make-camera (name x y w h)
  (make-instance '<camera>
		 :name name
		 :x x
		 :y y
		 :target-x x
		 :target-y y
		 :w w
		 :h h))

(defgeneric get-rect (camera)
  (:documentation "return sdl2 rect"))


(defmethod get-rect ((camera <camera>))
  (sdl2:make-rect (floor (camera-x camera))
		  (floor (camera-y camera))
		  (floor (camera-w camera))
		  (floor (camera-h camera))))

(defun get-camera-rectangle (camera)
  (make-rectangle :x (camera-x camera)
		  :y (camera-y camera)
		  :w (camera-w camera)
		  :h (camera-h camera)))


(defun camera/follow (camera pos vdir hdir)
  (let* ((org-x (vec2-x pos))
	 (org-y (vec2-y pos))
	 (max-x (camera-max-x camera))
	 (max-y (camera-max-y camera))
	 (height (camera-h camera))
	 (width (camera-w camera)))
    (setf (camera-target-y camera) 
	  (cond ((string= 'up vdir)
		 (min (- max-y height)
		      (max 0
			   (- org-y (* 0.6 height)))))
		((string= 'down vdir)
		 (max 0
		      (min (- max-y height)
			   (- org-y (* 0.4 height)))))
		(t org-y)))

    (setf (camera-target-x camera)
	  (cond ((string= 'left hdir)
		 (min (- max-x width)
		      (max 0 
			   (- org-x (* 0.6 width)))))
		((string= 'right hdir)
		 (max 0
		      (min (- max-x width)
			   (- org-x (* 0.4 width)))))
		(t org-x)))))
			 

(defun camera/update (camera dt)
  (let* ((target-x (camera-target-x camera))
	 (target-y (camera-target-y camera))
	 (camera-x (camera-x camera))
	 (camera-y (camera-y camera))
	 (delta-x (- camera-x target-x))
	 (delta-y (- camera-y target-y)))
    (when (or (not (equal camera-x target-x))
	      (not (equal camera-y target-y)))
      (setf (camera-x camera)
	    (- (camera-x camera)
	       (/ (* delta-x dt) 1000)))
      (setf (camera-y camera)
	    (- (camera-y camera)
	       (/ (* delta-y dt) 1000))))))
    

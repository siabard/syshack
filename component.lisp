(in-package #:syshack)

;;;; 위치 position 
(defclass <cposition> ()
  ((x :accessor cposition-x
      :initarg :x
      :initform 0)
   (y :accessor cposition-y
      :initarg :y
      :initform 0)))


(defun make-position-component (x y)
  (make-instance '<cposition>
		 :x x 
		 :y y))

;;;; 크기 size
(defclass <csize> ()
  ((w :accessor csize-w
      :initarg :w
      :initform 0)
   (h :accessor csize-h
      :initarg :h
      :initform 0)))

(defun make-size-component (w h)
  (make-instance '<csize>
		 :w w
		 :h h))

;;;; 이동 movement
(defclass <cmovement> ()
  ((x :accessor cmovement-x 
      :initarg :x
      :initform 0)
   (y :accessor cmovement-y
      :initarg :y
      :initform 0)))

(defun make-movement-component (x y)
  (make-instance '<cmovement>
		 :x x
		 :y y))

;;;; 애니메이션 animation 
(defclass <canimation> ()
  ((animations :accessor canimation-animations
	       :initarg :animations)
   (current-frame :accessor canimation-current-frame
		  :initarg :current-frame
		  :initform 0)
   (current-time :accessor canimation-current-time
		 :initarg :current-time
		 :initform 0)
   (current-animation :accessor canimation-current-animation
		      :initarg :current-animation
		      :initform nil)
   (ended? :accessor canimation-ended?
	   :initarg :ended?
	   :initform nil)
   (repeat? :accessor canimation-repeat?
	    :initarg :repeat?)))
  

(defun make-animation-component (repeat?)
  (let* ((animations (make-hash-table :test 'equal)))
    (make-instance '<canimation>
		   :animations animations
		   :current-frame 0
		   :ended? nil
		   :repeat? repeat?)))



;;;; state
(defclass <cstate> ()
  ((name :accessor state-name
	 :initarg :name)))

(defun make-state-component (name)
  (make-instance '<cstate>
		 :name name))

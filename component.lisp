(in-package #:syshack)

;;;; 위치 position 
(defclass <cposition> ()
  ((x :accessor cposition-x
      :initarg :x
      :initform 0)
   (y :accessor cposition-y
      :initarg :y
      :initform 0)))


;;;; 크기 size
(defclass <csize> ()
  ((w :accessor csize-w
      :initarg :w
      :initform 0)
   (h :accessor csize-h
      :initarg :h
      :initform 0)))

;;;; 이동 movement
(defclass <cmovement> ()
  ((x :accessor cmovement-x 
      :initarg :x
      :initform 0)
   (y :accessor cmovement-y
      :initarg :y
      :initform 0)))

;;;; 애니메이션 animation 
(defclass <canimation> ()
  ((animations :accessor canimation-animations
	       :initarg :animations
	       :initform (make-hash-table :test 'equal))
   (current-frame :accessor canimation-current-frame
		  :initarg :current-frame
		  :initform 0)
   (ended? :accessor canimation-ended?
	   :initarg :ended?
	   :initform nil)
   (repeat? :accessor canimation-repeat?
	    :initarg :repeat?
	    :initform nil)))


;;;; state
(defclass <cstate> ()
  ((name :accessor state-name
	 :initarg :name)))

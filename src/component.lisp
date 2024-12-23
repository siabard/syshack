(in-package #:syshack)

;;;; 위치 position 
(defclass <cposition> ()
  ((x :accessor cposition-x
      :initarg :x
      :initform 0)
   (y :accessor cposition-y
      :initarg :y
      :initform 0)
   (prev-x :accessor cposition-prev-x
	   :initarg :prev-x
	   :initform 0)
   (prev-y :accessor cposition-prev-y
	   :initarg :prev-y
	   :initform 0)))


(defun make-position-component (x y)
  (make-instance '<cposition>
		 :x x 
		 :y y
		 :prev-x x
		 :prev-y y))




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


;;;; 키보드 입력에 대응하는 컴포넌트
;;;; cinput 의 각 기능은 boolean 값이다
;;;; 키가 눌리면(pressed) -> T
;;;; 키가 놓이면(released) -> F

(defclass <cinput> ()
  ((up :accessor cinput-up
	:initarg :up
	:initform nil)
   (down :accessor cinput-down
	 :initarg :down
	 :initform nil)
   (left :accessor cinput-left
	 :initarg :left
	 :initform nil)
   (right :accessor cinput-right
	  :initarg :right
	  :initform nil)
   (shoot :accessor cinput-shoot
	  :initarg :shoot
	  :initform nil)))


(defun make-input-component ()
  (make-instance '<cinput>))


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

;;;; facing
(defclass <cfacing> ()
  ((hdir :initarg :hdir
	 :accessor cfacing-hdir
	 :initform 'none)
   (vdir :initarg :vdir
	 :accessor cfacing-vdir
	 :initform 'none)))

(defun make-facing-component ()
  (make-instance '<cfacing>
		 :hdir 'none
		 :vdir 'none))

(defun cfacing/set-vdir (cfacing vdir)
  (setf (cfacing-vdir cfacing) vdir))

(defun cfacing/set-hdir (cfacing hdir)
  (setf (cfacing-hdir cfacing) hdir))

;;;; trigger
(defclass <ctrigger> ()
  ((on-enter :initarg :on-enter
	     :accessor ctrigger-on-enter
	     :initform nil)
   (on-exit :initarg :on-exit
	    :accessor ctrigger-on-exit
	    :initform nil)))

(defun make-trigger-component ()
  (make-instance '<ctrigger>))

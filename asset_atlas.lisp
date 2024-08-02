(in-package #:syshack)

(defclass <atlas> ()
  ((name :accessor atlas-name
	 :initarg :name
	 :type string)
   (texture-name :accessor atlas-texture-name
		 :initarg :texture-name)
   (tile-width :accessor atals-tile-width
	       :initarg :tile-width)
   (tile-height :accessor atlas-tile-height
		:initarg :tile-height)
   (tile-row :accessor atlas-tile-row
	     :initarg :row
	     :initform 0)
   (tile-col :accessor atlas-tile-col
	     :initarg :col
	     :initform 0)))


;;;; Atlas 생성시에 해당 텍스쳐 정보가 필요하다.
;;;; 텍스쳐의 가로 , 세로 크기가 있어야하는데 외부에서 넣어주는 것이 좋을까?
;;;; 외부에서 넣자..
(defun make-atlas (name texture-name texture-width texture-height tile-width tile-height)
  (let* ((tile-col (floor texture-width tile-width))
	 (tile-row (floor texture-height tile-height)))
    (make-instance '<atlas>
		   :name name
		   :texture-name texture-name
		   :tile-width tile-width
		   :tile-height tile-height
		   :tile-col tile-col
		   :tile-row tile-row)))
	       
		 

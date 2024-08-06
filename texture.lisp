(in-package #:syshack)

(defclass <ctexture> ()
  ((texture :accessor ctexture-texture
	    :initarg :texture)
   (w :accessor ctexture-w
      :initarg :w)
   (h :accessor ctexture-h 
      :initarg :h)
   (tile-width :accessor ctexture-tile-width
	       :initarg :tile-width)
   (tile-height :accessor ctexture-tile-height
		:initarg :tile-height)
   (col :accessor ctexture-col
	:initarg :col)
   (row :accessor ctexture-row 
	:initarg :row)
   (atlas :accessor ctexture-atlas
	  :initarg :atlas 
	  :initform nil)))


;; 1개 타일의 가로와 세로, 열의 갯수와 행의 갯수를 토대로
;; atlas 정보를 생성한다.
;; atlas 정보는 리스트의 배열이어야한다  ((x y w h) .... )
(defun make-atlas (tile-width tile-height col row)
  (let ((atlas-list (loop for y from 0 to (- row 1) 
			  append (loop for x from 0 to (- col 1) 
				       collect  (list (* x tile-width)
						      (* y tile-height)
						      tile-width
						      tile-height)))))
    (make-array (length atlas-list) :initial-contents atlas-list)))
	     

(defun make-texture (renderer path tile-width tile-height)
  (let* ((texture (load-texture renderer path)))
    (multiple-value-bind (format access width height) (sdl2:query-texture texture)
      (let* ((tile-col (floor width tile-width))
	     (tile-row (floor height tile-height))
	     (atlas (make-atlas tile-width tile-height tile-col tile-row)))
	(make-instance '<ctexture>
		       :texture texture 
		       :w width
		       :h height
		       :tile-width tile-width
		       :tile-height tile-height 
		       :col tile-col 
		       :row tile-row
		       :atlas atlas)))))

(defgeneric release-texture (ctexture)
  (:documentation "release texture when quit"))

(defmethod release-texture ((ctexture <ctexture>))
  (let ((texture (ctexture-texture ctexture)))
    (safe-delete-texture texture)))


;;;; safe delete texture
(defun safe-delete-texture (texture)
  (when (autowrap:valid-p texture)
    (sdl2:destroy-texture texture)))


;;; load texture
(defun load-texture (renderer path)
  (let* ((surface (sdl2-image:load-image path))
	 (texture (sdl2:create-texture-from-surface renderer surface)))
    (progn
      (sdl2:free-surface surface)
      texture)))

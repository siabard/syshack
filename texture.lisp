(in-package #:syshack)


(defclass <ctexture> ()
  ((texture :accessor ctexture-texture
	    :initarg :textrue)
   (w :accessor ctexture-w
      :initarg :w)
   (h :accessor ctexture-h 
      :initarg :h)))

(defun make-texture (renderer path)
  (let* ((texture (load-texture renderer path)))
    (multiple-value-bind (format access width height) (sdl2:query-texture texture)
      (make-instance '<ctexture>
		     :texture texture 
		     :w width
		     :h height))))

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

(in-package #:syshack)

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

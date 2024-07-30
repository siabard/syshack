(in-package #:syshack)

;;;; safe delete texture
(defun safe-delete-texture (texture)
  (when (autowrap:valid-p texture)
    (sdl2:destroy-texture texture)))


;;;; safe close font
(defun safe-close-font (font)
  (when (autowrap:valid-p font)
    (sdl2-ttf:close-font font)))


;;; load texture

(defun load-texture (renderer path)
  (let* ((surface (sdl2-image:load-image path))
	 (texture (sdl2:create-texture-from-surface renderer surface)))
    (progn
      (sdl2:free-surface surface)
      texture)))

(in-package #:syshack)

;;;; Sprite class
;; sprite class 는 다음과 같은 형태이다.
;; (make-sprite texture latlas)
;; 텍스쳐 데이터와 아틀라스 리스트를 가진다.


(defclass <sprite> ()
    ((texture :initarg :texture
	      :accessor sprite-texture
	      :initform nil)
     (atlases :initarg :atlases
	      :accessor sprite-atlases
	      :initform '())))

(defun make-sprite (&key texture atlases)
  (make-instance '<sprite>
		 :texture texture
		 :atlases atlases))

(defun make-sprite-from-ctexture (ctexture)
  (let* ((texture (ctexture-texture ctexture))
	 (atlases (ctexture-atlas ctexture)))
    (make-sprite :texture texture
		 :atlases atlases)))


(defun sprite/get-width-height (sprite &key (index 0))
  (let* ((atlases (sprite-atlases sprite))
	 (atlas (aref atlases index)))
    (values (caddr atlas)
	    (cadddr atlas))))
 	 


;;;; sprite rendering 한다.
(defun sprite/render (sprite renderer &key dest-rect (index 0))
  (let* ((texture (sprite-texture sprite))
	 (atlases (sprite-atlases sprite))
	 (atlas (aref atlases index))
	 (src-rect (list-to-sdl2-rect atlas)))
    (sdl2:render-copy-ex renderer 
			 texture
			 :source-rect src-rect
			 :dest-rect dest-rect
			 :angle 0.0
			 :center (sdl2:make-point 0 0)
			 :flip nil)))

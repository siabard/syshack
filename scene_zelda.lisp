(in-package #:syshack)

(defclass <scene-zelda> (<scene>)
  ((player :accessor scene-zelda-player
	   :initarg :player
	   :initform nil)))


;;;; 생성하기 
(defun scene/make-zelda (game)
  (let* ((em (make-entity-manager)))
    (make-instance '<scene-zelda>
		   :name "zelda"
		   :game game
		   :entity-manager em)))

		 
  
;;;; update 
(defmethod scene/update ((scene <scene-zelda>) dt) 
  ())

;;;; render 
						    
(defmethod scene/render ((scene <scene-zelda>))
  ())

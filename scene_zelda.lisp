(in-package #:syshack)

(defclass <scene-zelda> (<scene>)
  ((gamemap :accessor scene-zelda-gamemap
	    :initarg :gamemap)
   (camera :accessor scene-zelda-camera
	   :initarg :camera)
   (player :accessor scene-zelda-player
	   :initarg :player
	   :initform nil)))


;;;; 생성하기 
(defun scene/make-zelda (game)
  (let* ((em (make-entity-manager))
	 (camera (make-camera "default" 0 0 640 480)))
    (make-instance '<scene-zelda>
		   :name "zelda"
		   :game game
		   :camera camera
		   :entity-manager em)))

		 
  
;;;; 초기화하기 
(defmethod scene/init ((scene <scene-zelda>) path)
  (let* ((gm (scene-game scene))
	 (em (scene-entity-manager scene))
	 (am (game-asset-manager gm))
	 (in (open path)))
    (loop for line = (read-line in nil)
	  while line do
	    (let* ((splited (cl-ppcre:split "\\s+" line))
		   (cate (car splited)))
	      (cond ((string= cate "entity")
		     (let* ((entity-name (nth 1 splited))
			    (entity-tag (nth 2 splited))
			    (animation-name (nth 3 splited))
			    (w (nth 4 splited))
			    (h (nth 5 splited))
			    (gx (nth 6 splited))
			    (gy (nth 7 splited)))
		       (entity-manager/add-entity em entity-tag entity-name)
		    (t nil))))))
;;;; update 
(defmethod scene/update ((scene <scene-zelda>) dt) 
  ())

;;;; render 
						    
(defmethod scene/render ((scene <scene-zelda>))
  (format t "render zelda~%"))

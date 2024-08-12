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

		 
  
;;;; 테스트를 위해 player를 먼저 만들어 등록한다.
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
			    (gx (nth 4 splited))
			    (gy (nth 5 splited)))
		       (entity-manager/add-entity em entity-tag entity-name)))
		    ((string= cate "player")
		     (let* ((entity-name (nth 1 splited))
			    (entity-tag (nth 2 splited))
			    (gx (nth 3 splited))
			    (gy (nth 4 splited))
			    (position-component (make-position-component (* gx 16)
									 (* gy 16)))
			    (animations (asset-manager-animations am))
			    (animation-component (make-animation-component T))
			    (component-animations (canimation-animations animation-component))
			    (moveleft (gethash "moveleft" animations))
			    (moveright (gethash "moveright" animations))
			    (player (entity-manager/add-entity em entity-tag entity-name)))
			    
		       (setf (gethash "moveleft" component-animations) moveleft)
		       (setf (gethash "moveright" component-animations) moveright)
		       (setf (entity-animation player) component-animations)
		       (setf (entity-position player) position-component)))
		    (t nil))))
    (close in)))
;;;; update 
(defmethod scene/update ((scene <scene-zelda>) dt) 
  ())

;;;; render 
						    
(defmethod scene/render ((scene <scene-zelda>))
  ()
  )



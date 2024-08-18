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
  (format t "zelda init")
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
			    (gx (parse-integer (nth 4 splited)))
			    (gy (parse-inteer (nth 5 splited))))
		       (entity-manager/add-entity em entity-tag entity-name)))
		    ((string= cate "player")
		     (let* ((entity-name (nth 1 splited))
			    (entity-tag (nth 2 splited))
			    (gx (parse-integer (nth 3 splited)))
			    (gy (parse-integer (nth 4 splited)))
			    (position-component (make-position-component (* gx 16)
									 (* gy 16)))
			    (animations (asset-manager-animations am))
			    (animation-component (make-animation-component T))
			    (canimations (canimation-animations animation-component))
			    (moveleft (gethash "moveleft" animations))
			    (moveright (gethash "moveright" animations))
			    (player (entity-manager/add-entity em entity-tag entity-name)))
			    
		       (setf (gethash "moveleft" canimations) moveleft)
		       (setf (gethash "moveright" canimations) moveright)
		       (setf (canimation-current-animation animation-component) "moveleft")
		       (setf (entity-animation player) animation-component)
		       (setf (entity-position player) position-component)))
		    (t nil))))
    (close in)))

;;;; update 
(defmethod scene/update ((scene <scene-zelda>) dt) 
  (let* ((entities (entity-manager/get-entities (scene-entity-manager scene) nil)))
    (format t "~A~%" (length entities))))

;;;; render 
;;; animation / position 항목이 있는 내역에 대해 출력처리						    
(defmethod scene/render ((scene <scene-zelda>))
  (let* ((entities-animation-position 
	   (remove-if-not 
	    #'(lambda (entity)
		(and 
		 (entity-animation entity)
		 (entity-position entity)))
	    (entity-manager/get-entities (scene-entity-manager scene) nil))))
    (loop for entity in entities-animation-position do
      (let* ((position (entity-position entity))
	     (canimation (entity-animation entity))
	     (animations (canimation-animations canimation))
	     (current-animation (canimation-current-animation canimation))
	     (animation (gethash current-animation animations))
	     (texture-name (animation-texture-name animation))
	     (game (scene-game scene))
	     (renderer (game-renderer game))
	     (asset-manager (game-asset-manager game))
	     (texture-asset (asset-manager-textures asset-manager))
	     (texture (gethash texture-name texture-asset))
	     (texture-texture (ctexture-texture texture))
	     (atlas (ctexture-atlas texture))
	     (src-rect (list-to-sdl2-rect (aref atlas 0)))
	     (dst-rect (sdl2:make-rect (cposition-x position)  
				       (cposition-y position)
				       16 
				       16)))
             
	(sdl2:render-copy-ex renderer texture-texture
			     :source-rect src-rect
			     :dest-rect dst-rect
			     :angle 0
			     :center (sdl2:make-point 0 0)
			     :flip nil)
	  ))))



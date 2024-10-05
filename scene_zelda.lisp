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
	 (camera (make-camera "default" 16 16 640 480)))
    (make-instance '<scene-zelda>
		   :name "zelda"
		   :game game
		   :gamemap (make-hash-table :test 'equal)
		   :camera camera
		   :entity-manager em)))



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
			    (gy (parse-integer (nth 5 splited)))
			    (position-component (make-position-component (* gx 16)
									 (* gy 16)))
			    (animations (asset-manager-animations am))
			    (animation-component (make-animation-component T))
			    (canimations (canimation-animations animation-component))
			    (new-entity (entity-manager/add-entity em entity-tag entity-name)))
		       (setf (gethash animation-name canimations)
			     (gethash animation-name animations))
		       (setf (canimation-current-animation animation-component) animation-name)
		       (setf (entity-animation new-entity) animation-component)
		       (setf (entity-position new-entity) position-component)))


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
		    ((string= cate "map")
		     (let* ((map-name (nth 1 splited))
			    (map-path (nth 2 splited))
			    (game-map (make-tiled-map am map-path)))
		       (setf (gethash map-name (scene-zelda-gamemap scene)) game-map)))

		    (t nil))))
    (close in)))

;;;; update
(defmethod scene/update ((scene <scene-zelda>) dt)
  (let* ((entities (entity-manager/get-entities (scene-entity-manager scene) nil)))
    (system/animation entities dt)))


;;;; animation system

(defun system/animation (entities dt)
  ;; canimation 이 있는 항목만 체크해야한다.
  (let* ((animation-entities (remove-if-not
			      #'(lambda (entity)
				  (entity-animation entity))
			      entities)))
    (loop for entity in animation-entities do
      (let* ((canimation (entity-animation entity))
	     (animations (canimation-animations canimation))
	     (animation-current-animation-key (canimation-current-animation canimation))
	     (current-animation (gethash animation-current-animation-key animations))
	     (frame-length (animation-frame-length current-animation))
	     (animation-current-frame (canimation-current-frame canimation))
	     (animation-current-time (canimation-current-time canimation))
	     (next-animation-current-time (+ animation-current-time dt)))
	(cond ((> next-animation-current-time 240)
	       (progn
		 (incf animation-current-frame)
		 (cond ((>= animation-current-frame frame-length)
			(setf (canimation-current-frame canimation) 0))
		       (t
			(setf (canimation-current-frame canimation)
			      animation-current-frame)))
		 (setf (canimation-current-time canimation) 0)))
	      (t
	       (setf (canimation-current-time canimation)
		     next-animation-current-time)))))))





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
      (let* ((cposition (entity-position entity))
	     (canimation (entity-animation entity))
	     (current-frame (canimation-current-frame canimation))
	     (animations (canimation-animations canimation))
	     (current-animation (canimation-current-animation canimation))
	     (animation (gethash current-animation animations))
	     (texture-name (animation-texture-name animation))
	     (game (scene-game scene))
	     (renderer (game-renderer game))
	     (camera (scene-zelda-camera scene))
	     (camera-rectangle (get-camera-rectangle camera))
	     (asset-manager (game-asset-manager game))
	     (texture-asset (asset-manager-textures asset-manager))
	     (texture (gethash texture-name texture-asset))
	     (texture-texture (ctexture-texture texture))
	     (t-atlas (ctexture-atlas texture))
	     (atlas (aref t-atlas current-frame))
	     (atlas-x (car atlas))
	     (atlas-y (cadr atlas))
	     (atlas-w (caddr atlas))
	     (atlas-h (cadddr atlas))
	     (clipped-src-rectangle 
	       (clip-rect-src
		(make-rectangle :x (cposition-x cposition)
				:y (cposition-y cposition)
				:w atlas-w
				:h atlas-h)
		camera-rectangle))
	     (src-rect (sdl2:make-rect 
			(+ atlas-x
			   (rectangle-x clipped-src-rectangle))
			(+ atlas-y 
			   (rectangle-y clipped-src-rectangle))
			(rectangle-w clipped-src-rectangle) 
			(rectangle-h clipped-src-rectangle)))
	     (dst-rect (sdl2:make-rect (+ 
					(- (cposition-x cposition)
					   (camera-x camera))
					(- atlas-w (rectangle-w clipped-src-rectangle)))
				       (+
					(- (cposition-y cposition)
					   (camera-y camera))
					(- atlas-h (rectangle-h clipped-src-rectangle)))
				       (rectangle-w clipped-src-rectangle) 
				       (rectangle-h clipped-src-rectangle))))
	
	(scene-zelda/render-map scene)
	(sdl2:render-copy-ex renderer texture-texture
			     :source-rect src-rect
			     :dest-rect dst-rect
			     :angle 0
			     :center (sdl2:make-point 0 0)
			     :flip nil)
	))))




(defun scene-zelda/render-map (scene-zelda)
  (let* ((game (scene-game scene-zelda))
	 (renderer (game-renderer game))
	 (asset-manager (game-asset-manager game))
	 (map-table (get-map-from-game game))
	 (current-map (gethash "level1" map-table))
	 (camera (scene-zelda-camera scene-zelda))
	 (camera-rectangle (get-camera-rectangle camera))
	 (layers (tiled-map-layers current-map))
	 (layers-omit-collision (remove-if #'(lambda (layer)
					       (string=
						(cl-tiled:layer-name layer)
						"collision"))
					   layers)))

    (loop for layer in layers-omit-collision
	  do (let* ((cells (clip-layer-with-camera camera layer)))
	       (loop for cell in cells
		     do (let* ((first-gid (cl-tiled:tileset-first-gid
					   (cl-tiled:tile-tileset
					    (cl-tiled:cell-tile cell)))))
			  (multiple-value-bind (index texture-name)
			      (map-tile-info-map-texture current-map
							 (+ first-gid 
							    (cl-tiled:tile-id 
							     (cl-tiled:cell-tile cell))))
			    (let* ((cell-column (cl-tiled:cell-column cell))
				   (cell-row (cl-tiled:cell-row cell)))
			      (multiple-value-bind (texture atlas)
				  (get-map-texture-and-atlas asset-manager index texture-name)
				(let* ((clipped-src-rectangle (clip-rect-src 
							       (make-rectangle :x (* 32 cell-column)
									       :y (* 32 cell-row)
									       :w 32
									       :h 32)
							       camera-rectangle))
				       (src-rect (sdl2:make-rect 
						  (+ (car atlas) (rectangle-x clipped-src-rectangle))
						  (+ (cadr atlas) (rectangle-y clipped-src-rectangle))
						  (rectangle-w clipped-src-rectangle) 
						  (rectangle-h clipped-src-rectangle)))
				       (dst-rect (sdl2:make-rect (+ 
								  (- (* 32 cell-column)
								     (camera-x camera))
								  (- 32 (rectangle-w clipped-src-rectangle)))
								 (+
								  (- (* 32 cell-row)
								     (camera-y camera))
								  (- 32 (rectangle-h clipped-src-rectangle)))
								 (rectangle-w clipped-src-rectangle) 
								 (rectangle-h clipped-src-rectangle))))
				  (sdl2:render-copy-ex renderer
						       texture
						       :source-rect src-rect
						       :dest-rect dst-rect
						       :angle 0
						       :center (sdl2:make-point 0 0)
						       :flip nil)))))))))))

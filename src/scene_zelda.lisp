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
    (setf (camera-max-x camera) 2000
	  (camera-max-y camera) 1000)
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
			     (gethash animation-name animations)
			     (canimation-current-animation animation-component) animation-name
			     (entity-animation new-entity) animation-component
			     (entity-position new-entity) position-component)))


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
			    (size-component (make-size-component 16 16))
			    (moveleft (gethash "moveleft" animations))
			    (moveright (gethash "moveright" animations))
			    (input (make-input-component))
			    (movement (make-movement-component 0 0))
			    (facing (make-facing-component))
			    (player (entity-manager/add-entity em entity-tag entity-name)))

		       (setf (scene-zelda-player scene) player
			     (gethash "moveleft" canimations) moveleft
			     (gethash "moveright" canimations) moveright
			     (canimation-current-animation animation-component) "moveleft"
			     (entity-animation player) animation-component
			     (entity-movement player) movement
			     (entity-input player) input
			     (entity-position player) position-component
			     (entity-facing player) facing
			     (entity-size player) size-component)))
		    ((string= cate "map")
		     (let* ((map-name (nth 1 splited))
			    (map-path (nth 2 splited))
			    (game-map (make-tiled-map am map-path)))
		       (setf (gethash map-name (scene-zelda-gamemap scene)) game-map)
		       ;; collision 이라 설정된 영역은 collider에 추가함..
		       (let* ((layers (tiled-map-layers game-map))
			      (layers-collision (remove-if-not #'(lambda (layer)
								   (string=
								    (cl-tiled:layer-name layer)
								    "collision"))
							       layers)))
			 (loop for layer in layers-collision
			       do (let* ((cells (cl-tiled:layer-cells layer)))
				    (loop for cell in cells
					  do (let* ((cell-column (cl-tiled:cell-column cell))
						    (cell-row (cl-tiled:cell-row cell))
						    (x (* cell-column 32))
						    (y (* cell-row 32))
						    (w 32)
						    (h 32))
					       (scene-zelda/register-collision em
 									       (make-position-component x y)
									       (make-size-component w h)))))))))
		    (t nil))))
    (scene/register-action scene 80 "LEFT")
    (scene/register-action scene 79 "RIGHT")
    (scene/register-action scene 81 "DOWN")
    (scene/register-action scene 82 "UP")
    (close in)))


;;; entities 에 collision 맵 타일을 더한다.
;;; collision map tile은 entity로 취급할 수 있도록 한다.
;;; entity name 은 tile, tag는 coll 로 한다.
(defun scene-zelda/register-collision  (entity-manager pos-comp size-comp)
  (let* ((entity-name "tile")
	 (entity-tag "coll")
	 (cell (entity-manager/add-entity entity-manager entity-tag entity-name)))
    (setf (entity-position cell) pos-comp)
    (setf (entity-size cell) size-comp)))

;;;; update
(defmethod scene/update ((scene <scene-zelda>) dt)
  (scene-zelda/do-input scene)
  (scene-zelda/do-movement scene dt)
  (scene-zelda/do-collision scene)
  (let* ((entities (entity-manager/get-entities (scene-entity-manager scene) nil)))
    (system/animation entities dt))
  (scene-zelda/do-camera scene dt))


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
			(floor (+ atlas-x
				  (rectangle-x clipped-src-rectangle)))
			(floor (+ atlas-y
				  (rectangle-y clipped-src-rectangle)))
			(floor (rectangle-w clipped-src-rectangle))
			(floor (rectangle-h clipped-src-rectangle))))
	     (dst-rect (sdl2:make-rect (floor (+
					       (- (cposition-x cposition)
						  (camera-x camera))
					       (- atlas-w (rectangle-w clipped-src-rectangle))))
				       (floor (+
					       (- (cposition-y cposition)
						  (camera-y camera))
					       (- atlas-h (rectangle-h clipped-src-rectangle))))
				       (floor (rectangle-w clipped-src-rectangle))
				       (floor (rectangle-h clipped-src-rectangle)))))

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
						  (floor (+ (car atlas) (rectangle-x clipped-src-rectangle)))
						  (floor (+ (cadr atlas) (rectangle-y clipped-src-rectangle)))
						  (floor (rectangle-w clipped-src-rectangle))
						  (floor (rectangle-h clipped-src-rectangle))))
				       (dst-rect (sdl2:make-rect (floor (+
									 (- (* 32 cell-column)
									    (camera-x camera))
									 (- 32 (rectangle-w clipped-src-rectangle))))
								 (floor (+
									 (- (* 32 cell-row)
									    (camera-y camera))
									 (- 32 (rectangle-h clipped-src-rectangle))))
								 (floor (rectangle-w clipped-src-rectangle))
								 (floor (rectangle-h clipped-src-rectangle)))))
				  (sdl2:render-copy-ex renderer
						       texture
						       :source-rect src-rect
						       :dest-rect dst-rect
						       :angle 0
						       :center (sdl2:make-point 0 0)
						       :flip nil)))))))))))



(defmethod scene/do-action ((scene <scene-zelda>) code act)
  (let* ((actionmap (scene-actionmap scene))
	 (caction (gethash code actionmap))
	 (player (scene-zelda-player scene))
	 (player-input (entity-input player)))
    (when (not (null caction))
      (cond ((string= act "START")
	     (cond ((string= caction "LEFT")
		    (setf (cinput-left player-input) T))
		   ((string= caction "RIGHT")
		    (setf (cinput-right player-input) T))
		   ((string= caction "UP")
		    (setf (cinput-up player-input) T))
		   ((string= caction "DOWN")
		    (setf (cinput-down player-input) T))))
	    ((string= act "STOP")
	     (cond ((string= caction "LEFT")
		    (setf (cinput-left player-input) nil))
		   ((string= caction "RIGHT")
		    (setf (cinput-right player-input) nil))
		   ((string= caction "UP")
		    (setf (cinput-up player-input) nil))
		   ((string= caction "DOWN")
		    (setf (cinput-down player-input) nil))))))))



(defun scene-zelda/do-input (scene)
  (let* ((player (scene-zelda-player scene))
	 (player-input (entity-input player))
	 (player-movement (entity-movement player))
	 (player-facing (entity-facing player))
	 (speed 60)
	 (init-vec2 (make-vec2 :x 0 :y 0)))
    (when (cinput-up player-input)
      (cfacing/set-vdir player-facing 'up)
      (setf init-vec2 (vec2+ init-vec2 (make-vec2 :x 0 :y -1))))
    (when (cinput-down player-input)
      (cfacing/set-vdir player-facing 'down)
      (setf init-vec2 (vec2+ init-vec2 (make-vec2 :x 0 :y 1))))
    (when (cinput-left player-input)
      (cfacing/set-hdir player-facing 'left)
      (setf init-vec2 (vec2+ init-vec2 (make-vec2 :x -1  :y 0))))
    (when (cinput-right player-input)
      (cfacing/set-hdir player-facing 'right)
      (setf init-vec2 (vec2+ init-vec2 (make-vec2 :x 1 :y 0))))
    (let ((norm (vec2-scale (vec2-normalize init-vec2) speed)))
      (setf (cmovement-x player-movement) (vec2-x norm))
      (setf (cmovement-y player-movement) (vec2-y norm)))))


(defun scene-zelda/do-movement (scene dt)
  (let* ((float-dt (/ dt 1000))
	 (entities-movement-position
	   (remove-if-not
	    #'(lambda (entity)
		(and
		 (entity-movement entity)
		 (entity-position entity)))
	    (entity-manager/get-entities (scene-entity-manager scene) nil))))
    (loop for entity in entities-movement-position do
      (let ((position (entity-position entity))
	    (movement (entity-movement entity)))
	(setf (cposition-prev-x position)
	      (cposition-x position))
	(setf (cposition-prev-y position)
	      (cposition-y position))
	(setf (cposition-x position)
	      (+ (cposition-x position)
		 (* (cmovement-x movement) float-dt)))
	(setf (cposition-y position)
	      (+ (cposition-y position)
		 (* (cmovement-y movement) float-dt)))))))



(defun scene-zelda/do-collision (scene-zelda)
  (let* ((player (scene-zelda-player scene-zelda))
	 (entity-manager (scene-entity-manager scene-zelda))
	 (player-pos (entity-position player))
	 (colliders (entity-manager/get-entities entity-manager "coll")))
    ;; Collision entity와의 충돌확인
    (loop for collider in colliders
	  do (let* ((collision-direction (entity/collide-direction player collider))
		    (coll-amount (entity/position-overlap player collider)))
	       (when (and
		      (> (vec2-x coll-amount) 0)
		      (> (vec2-y coll-amount) 0))
		 (when (string= collision-direction 'up)
		   (setf (cposition-y player-pos)
			 (+ (cposition-y player-pos)
			    (vec2-y coll-amount))))
		 (when (string= collision-direction 'down)
		   (setf (cposition-y player-pos)
			 (- (cposition-y player-pos)
			    (vec2-y coll-amount))))
		 (when (string= collision-direction 'left)
		   (setf (cposition-x player-pos)
			 (+ (cposition-x player-pos)
			    (vec2-x coll-amount))))
		 (when (string= collision-direction 'right)
		   (setf (cposition-x player-pos)
			 (- (cposition-x player-pos)
			    (vec2-x coll-amount)))))))))


(defun scene-zelda/do-camera (scene-zelda dt)
  (let* ((camera (scene-zelda-camera scene-zelda))
	 (player (scene-zelda-player scene-zelda))
	 (player-facing (entity-facing player))
	 (vdir (cfacing-vdir player-facing))
	 (hdir (cfacing-hdir player-facing))
	 (player-pos (entity-position player))
	 (pos (make-vec2 :x (cposition-x player-pos)
			 :y (cposition-y player-pos))))
    (camera/follow camera pos vdir hdir)
    (camera/update camera dt)))

;;;; Game class 를 정의한다.
;;;; Game class는 다음과 같은 클래스이다.
;;;; (make-instance 'game window renderer)

(in-package #:syshack)

(defclass <game> ()
  ((name :accessor game-name
	 :initarg :name
	 :type string
	 :documentation "Game's name")
   (current-scene :accessor game-current-scene
		  :initarg :current-scene
		  :type string
		  :documentation "Game's current scene")
   (scenes :accessor game-scenes
	   :initarg :scenes
	   :documentation "hash table of scenes")
   (asset-manager :accessor game-asset-manager
		  :initarg :asset-manager
		  :documentation "Asset Manager")
   (gui-manager :accessor game-gui-manager
		:initarg :gui-manager
		:documentation "Gui Manager")
   (renderer :accessor game-renderer
	     :initarg :renderer
	     :initform nil
	     :documentation "Game's window renderer")
   (key-input :accessor game-key-input
	      :initarg :key-input)
   (panel :accessor game-panel
	  :initform nil
	  :initarg :panel
	  :documentation "샘플 Panel - 나중에 지웁니다.")
   (sprite :accessor game-sprite
	   :initform nil
	   :initarg :sprite
	   :documentation "샘플 Sprite - 나중에 지웁니다.")
   (dialog :accessor game-dialog
	   :initform nil
	   :initarg :dialog)
   (window :accessor game-window
	   :initarg :window
	   :initform nil
	   :documentation "Game's window"))
  (:documentation "Game object"))

(defun make-game (name window renderer)
  (let* ((asset-manager (make-asset-manager renderer))
	 (gui-manager (make-gui-manager))
	 (key-input (make-key-input))
	 (scenes (make-hash-table :test 'equal)))
    (make-instance '<game>
		   :name name
		   :window window
		   :renderer renderer
		   :key-input key-input
		   :scenes scenes
		   :gui-manager gui-manager
		   :current-scene ""
		   :asset-manager asset-manager)))


;; game 초기화 모듈 

(defgeneric game/init (game path)
  (:documentation "initailize game system with config file"))

(defmethod game/init (game path)
  (let* ((am (game-asset-manager game))
	 (in (open path))
	 (zelda (scene/make-zelda game))
	 (scenes (game-scenes game)))
    (loop for line = (read-line in nil)
	  while line do 
	    (let* ((splited (cl-ppcre:split "\\s+" line))
		   (cate (car splited))
		   (name (cadr splited))
		   (path (caddr splited)))
	      (cond ((string= cate "font")
		     (asset-manager/add-font am name path))
		    ((string= cate "map")
		     (asset-manager/add-gamemap am name path))
		    ((string= cate "bitmap")
		     (let ((width (parse-integer (nth 3 splited)))
			   (height (parse-integer (nth 4 splited))))
		       (asset-manager/add-texture am name path width height)))
		    ((string= cate "animation")
		     (let* ((texture-name (nth 2 splited))
			    (start-frame (parse-integer (nth 3 splited)))
			    (frame-length (parse-integer (nth 4 splited)))
			    (animation (make-animation 
					name 
					texture-name 
					start-frame 
					frame-length)))
		       (asset-manager/add-animation am name animation)))
		    (t
		     nil))))
    (close in)
    (let* ((panel (make-panel (asset-manager/get-texture am "panel")))
	   (guim (game-gui-manager game)))
      (gui/add-dialog guim "t1" 120 60 240 140 panel "test"
		      '("우리는 미래를 위해 움직여야합니다."
			"새로운 게임 세상을 만들 것입니다."
			"놀라운 미래가 기다립니다.")
		      nil nil)
      (setf (gethash "zelda" scenes) zelda
	    (game-current-scene game) "zelda"
	    *current-scene* zelda)
      (let* ((player-texture (asset-manager/get-texture am "player"))
	     (sprite (make-sprite-from-ctexture player-texture)))
	(setf (game-sprite game) sprite))
      (init-keys (game-key-input game))
      
      (scene/init zelda "./resources/level/level1.txt"))))



(defun file-io-test (path)
  (let ((in (open path)))
    (when in 
      (loop for line = (read-line in nil)
	    while line do (format t "~a~%" line))
      (close in))))

;; game loop 모듈
(defgeneric game/loop (game)
  (:documentation "EVENT LOOP"))

(defmethod game/loop (game)
  (let* ((start-tick (sdl2:get-ticks))
	 (end-tick (sdl2:get-ticks))
	 (dt (- end-tick start-tick))
	 (key-input (game-key-input game))
	 (current-scene-name (game-current-scene game))
	 (scenes (game-scenes game))
	 (scene (gethash current-scene-name scenes)))
    (sdl2:with-event-loop (:method :poll)
      (:idle ()
	     (let* ((renderer (game-renderer game))
		    (key-input (game-key-input game)))
	       (when (key-pressed-p key-input (sdl2:scancode-key-to-value :scancode-escape))
		 (sdl2:push-event :quit))
	       (setf start-tick (sdl2:get-ticks))
	       (setf dt (- start-tick end-tick))
	       (setf end-tick start-tick)
	       (sdl2:render-clear renderer)
	       (game/update game dt)
	       (game/render game)
	       (sdl2:render-present renderer)
	       (clear-keys key-input)
	       (when (< dt 16)
		 (sdl2:delay (- 16 dt)))))
      (:keyup (:keysym keysym)
	      (let* ((scancode (sdl2:scancode-value keysym)))
		(scene/do-action scene scancode "STOP")
		(keyup-event key-input scancode)))
      (:keydown (:keysym keysym)
		(let* ((scancode (sdl2:scancode-value keysym)))
		  (scene/do-action scene scancode "START")
		  (keydown-event key-input scancode)))
      (:quit ()
	     (game/quit game)
	     t))))


;; game의 state를 update한다.
(defgeneric game/update (game dt)
  (:documentation "update game state with delta time"))


(defmethod game/update ((game <game>) dt)
  (let* ((current-scene-name (game-current-scene game))
	 (scenes (game-scenes game))
	 (scene (gethash current-scene-name scenes)))
    (scene/update scene dt)))


;; game을 rendering 한다.
(defgeneric game/render (game)
  (:documentation "render entity in game state"))

(defmethod game/render (game)
  (let* ((renderer (game-renderer game))
	 (am (game-asset-manager game))
	 (guim (game-gui-manager game))
	 (fonts (asset-manager-fonts am))
	 (ascii-bitmap-font (gethash "ascii" fonts))
	 (korean-bitmap-font (gethash "korean" fonts))
	 (sprite (game-sprite game))
	 (current-scene-name (game-current-scene game))
	 (scenes (game-scenes game))
	 (scene (gethash current-scene-name scenes)))
    (scene/render scene)
    (sprite/render sprite renderer 
		   :dest-rect (sdl2:make-rect 0 0 16 16))
    (draw-hangul renderer korean-bitmap-font (color-to-byte 255 0 0 255))
    (draw-string renderer  32 32 "안녕하세요" 
		 :korean-bitmap-font korean-bitmap-font
		 :ascii-bitmap-font ascii-bitmap-font
		 :color (color-to-byte 0 255 0 255))
    (draw-string renderer  32 48 "This is text" 
		 :korean-bitmap-font korean-bitmap-font
		 :ascii-bitmap-font ascii-bitmap-font)
    (draw-string renderer  32 64 "한 / 영 혼합" 
		 :korean-bitmap-font korean-bitmap-font
		 :ascii-bitmap-font ascii-bitmap-font)
    (gui/render-dialog guim renderer korean-bitmap-font ascii-bitmap-font)))

;; game 을 종료한다.
;; 모든 리소스를 free 한다.
(defgeneric game/quit (game)
  (:documentation "quit game"))

(defmethod game/quit ((game <game>))
  (let* ((am (game-asset-manager game))
	 (key-input (game-key-input game)))
    (quit-keys key-input)
    (asset-manager/cleanup am)))

;;;; Game class 를 정의한다.
;;;; Game class는 다음과 같은 클래스이다.
;;;; (make-instance 'game window renderer)

(in-package #:syshack)

(defclass <game> ()
  ((name :accessor game-name
	 :initarg :name
	 :type string
	 :documentation "Game's name")
   (asset-manager :accessor game-asset-manager
		  :initarg :asset-manager
		  :documentation "Asset Manager")
   (renderer :accessor game-renderer
	     :initarg :renderer
	     :initform nil
	     :documentation "Game's window renderer")
   (window :accessor game-window
	   :initarg :window
	   :initform nil
	   :documentation "Game's window"))
  (:documentation "Game object"))

(defun make-game (name window renderer)
  (let ((asset-manager (make-asset-manager renderer)))
    (make-instance '<game>
		   :name name
		   :window window
		   :renderer renderer
		   :asset-manager asset-manager)))


;; game 초기화 모듈 

(defgeneric game/init (game path)
  (:documentation "initailize game system with config file"))

(defmethod game/init (game path)
  (let* ((am (game-asset-manager game))
	 (in (open path)))
    (loop for line = (read-line in nil)
	  while line do 
	    (let* ((splited (cl-ppcre:split "\\s+" line))
		   (cate (car splited)))
	      (cond ((string= cate "font")
		     (asset-manager/add-font am (cadr splited) (caddr splited)))
		    (t
		     nil))
	      (format t "~a~%" cate)))
    (close in)))
    


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
  (sdl2:with-event-loop (:method :poll)
    (:idle ()
	   (let* ((renderer (game-renderer game)))
	     (sdl2:render-clear renderer)
	     (game/render game)
	     (sdl2:render-present renderer)))
    (:quit ()
	   (game/quit game)
	   t)))


;; game의 state를 update한다.
(defgeneric game/update (game dt)
  (:documentation "update game state with delta time"))


(defmethod game/update (game dt)
  ())


;; game을 rendering 한다.
(defgeneric game/render (game)
  (:documentation "render entity in game state"))

(defmethod game/render (game)
  (let* ((renderer (game-renderer game))
	 (am (game-asset-manager game))
	 (fonts (asset-manager-fonts am))
	 (ascii-bitmap-font (gethash "ascii" fonts))
	 (korean-bitmap-font (gethash "korean" fonts)))
    (when ascii-bitmap-font
      (draw renderer ascii-bitmap-font))
    (when korean-bitmap-font
      (draw-hangul renderer korean-bitmap-font))))

;; game 을 종료한다.
;; 모든 리소스를 free 한다.
(defgeneric game/quit (game)
  (:documentation "quit game"))

(defmethod game/quit ((game <game>))
  (let* ((am (game-asset-manager game)))
    (asset-manager/cleanup am)))

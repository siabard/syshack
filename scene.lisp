(in-package #:syshack)

(defclass <scene> ()
  ((name :accessor scene-name
	 :initarg :name
	 :type string)
   (game :accessor scene-game
	 :initarg :game)
   (entity-manager :accessor scene-entity-manager
		   :initarg :entity-manager)))


;; init scene
(defgeneric scene/init (scene path)
  (:documentation "Scene 초기화"))

;; update
(defgeneric scene/update (scene dt)
  (:documentation "Update scene with dt"))


(defmethod scene/update :after ((scene <scene>) dt)
  ;; entities 에서 삭제항목 지우고
  ;; 추가 entity append 하기
  (entity-manager/update (scene-entity-manager scene)))
	 

;; render 
(defgeneric scene/render (scene)
  (:documentation "Render scene"))


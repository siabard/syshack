(in-package #:syshack)

(defclass <scene> ()
  ((name :accessor scene-name
	 :initarg :name
	 :type string)
   (game :accessor scene-game
	 :initarg :game)
   (actionmap :accessor scene-actionmap
	      :initform (make-hash-table :test 'equal))
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


;; action map 등록하기
;; 키보드 코드와 결합한 action name 을 등록한다.
(defgeneric scene/register-action (scene code name)
  (:documentation "register scancode to action name"))

(defmethod scene/register-action (scene code name)
  (let ((actionmap (scene-actionmap scene)))
    (setf (gethash code actionmap) name)))

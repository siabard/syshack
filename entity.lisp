(in-package #:syshack)

;;;; enitty class

(defclass <entity-manager> ()
  ((entities :accessor entity-manager-entities
	     :initarg :entities
	     :initform nil)
   (added-entities :accessor entity-manager-added-entities
		   :initarg :added-entities
		   :initform nil))
  (:documentation "Entity Manager class"))

(defun make-entity-manager ()
  (make-instance '<entity-manager>))

(defgeneric entity-manager/add-entity (entity-manager tag name)
  (:documentation "add entity to entity-manager"))

(defmethod entity-manager/add-entity ((entity-manager <entity-manager>) tag name)
  (let* ((new-entity (make-entity tag name))
	 (added-entities (entity-manager-added-entities entity-manager)))
    (setf (entity-manager-added-entities entity-manager) (cons new-entity added-entities))
    new-entity))
;;;; entity

(defgeneric entity-manager/update (entity-manager)
  (:documentation "update entities"))

(defmethod entity-manager/update ((entity-manager <entity-manager>))
  (setf (entity-manager-entities entity-manager)
	(remove-if #'(lambda (entity) (not (entity-alive? entity)))
		   (entity-manager-entities entity-manager)))
  (let* ((added-entities (entity-manager-added-entities entity-manager)))
    (setf (entity-manager-entities entity-manager)
	  (append (entity-manager-entities entity-manager)
		  added-entities))
    (setf (entity-manager-added-entities entity-manager) nil)))


(defgeneric entity-manager/get-entities (entity-manager tag)
  (:documentation "retrieve entities"))

(defmethod entity-manager/get-entities ((entity-manager <entity-manager>) tag)
  (cond ((null tag)
	 (entity-manager-entities entity-manager))
	(t
	 (remove-if-not #'(lambda (entity)
			    (string= (entity-name entity) tag))
			(entity-manager-entities entity-manager)))))

(defclass <entity> ()
  ((sequence :accessor entity-sequence
	     :initform 0
	     :allocation :class)
   (id :accessor entity-id
       :initarg :id
       :initform 0)
   (tag :accessor entity-tag
	:initarg :tag
	:type string)
   (name :accessor entity-name
	 :initarg :name
	 :type string)
   (animation :accessor entity-animation
	      :initarg :animation
	      :initform nil)
   (position :accessor entity-position
	     :initarg :position
	     :initform nil)
   (alive? :accessor entity-alive?
	   :initarg :alive?
	   :initform T
	   :type boolean))

  (:documentation "Entity class"))

(defun make-entity (tag name)
  (let* ((new-entity (make-instance '<entity>
				    :name name
				    :tag tag))
	 (sequence (entity-sequence new-entity))
	 (next-seqnece (+ sequence 1)))
    (setf (entity-sequence new-entity) next-seqnece)
    (setf (entity-id new-entity) next-seqnece)
    new-entity))


(defgeneric entity/add-animation (entity animation)
  (:documentation "Add animation to entity"))

(defmethod entity/add-animation (entity animation)
  (setf (entity-animation entity) animation))


;;; entity에서 현재 animation의 현재 프레임관련 animation 정보를
;;; 반환한다.
(defgeneric entity/get-current-frame-animation (entity)
  (:documentation "해당 entity의 현재 애니메이션의 현재 프레임 반환"))

(defmethod entity/get-current-frame-animation (entity)
  (let* ((canimation (entity-animation entity))
	 (animations (canimation-animations canimation))   ;; hash-table
	 (current-animation (canimation-current-animation canimation)) ;; string
	 (animation (gethash current-animation animations))) ;; <animation>
    animation))

(defgeneric entity/get-current-animation-atlas (entity asset-manager)
  (:documentation "해당 entity 의 현재 애니메이션의 atlas 값"))

(defmethod entity/get-current-animation-atlas (entity asset-manager)
  (let* ((canimation (entity-animation entity))
	 (current-frame (canimation-current-frame canimation))
	 (animations (canimation-animations canimation))   ;; hash-table
	 (current-animation (canimation-current-animation canimation)) ;; string
	 (animation (gethash current-animation animations)) ;; <animation>
	 (texture-name (animation-texture-name animation))
	 (texture-asset (asset-manager-textures asset-manager))
	 (texture (gethash texture-asset texture-name))
	 (atlas (ctexture-atlas texture))
	 (current-frame-atlas (aref atlas current-frame)))
    current-frame-atlas))

;;; entity 에서 component를 통해 영역이 있는지 여부와
;;; 해당 영역 정보를 가져오기
;;; 해당 영역은 position 컴포넌트와 animation 으로 결정된다.
(defgeneric entity/get-bound-rect (entity asset-manager)
  (:documentation "entity 에 대한 bound 영역을 계산해서 알려준다."))

;;; bound box는 rectangle 형태이어야한다.
(defmethod entity/get-bound-rect (entity asset-manager)
  (let* ((cposition (entity-position entity))
	 (current-frame-atlas (entity/get-current-animation-atlas entity asset-manager)))
    (make-rectangle :x (cposition-x cposition)
		    :y (cposition-y cposition)
		    :w (aref current-frame-atlas 2)
		    :h (aref current-frame-atlas 3))))

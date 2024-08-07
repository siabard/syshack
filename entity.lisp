(in-package #:syshack)

;;;; enitty class

(defclass <entity-manager> ()
  ((entities :accessor entity-manager-entities
	     :initarg :entities
	     :initform nil))
  (:documentation "Entity Manager class"))

(defun make-entity-manager ()
  (make-instance '<entity-manager>))

(defgeneric entity-manager/add-entity (entity-manager tag name)
  (:documentation "add entity to entity-manager"))

(defmethod entity-manager/add-entity ((entity-manager <entity-manager>) tag name)
  (let* ((new-entity (make-entity tag name))
	 (entities (entity-manager-entities entity-manager)))
    (setf (entity-manager-entities entity-manager) (cons new-entity entities))))
;;;; entity 

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
	      :initarg animation
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

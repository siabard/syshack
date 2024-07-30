(in-package #:syshack)

;;;; enitty class

(defclass <entity-manager> ()
  ((entities :accessor entity-manager-entities
	     :initarg :entities
	     :initform nil))
  (:documentation "Entity Manager class"))

(defun make-entity-manager ()
  (make-instance '<entity-manager>))

(defgeneric entity-manager/add-entity (entity-manager tag)
  (:documentation "add entity to entity-manager"))

(defmethod entity-manager/add-entity ((entity-manager <entity-manager>) tag)
  (let* ((new-entity (make-entity tag))
	 (entities (entity-manager-entities entity-manager)))
    (setf (entity-manager-entities entity-manager) (cons new-entity entities))))
;;;; entity 

(defclass <entity> ()
  ((id :accessor entity-id 
       :initarg :id
       :initform 0
       :allocation :class)
   (tag :accessor entity-tag
	:initarg :tag
	:type string)
   (alive? :accessor entity-alive?
	   :initarg :alive?
	   :initform T
	   :type boolean))
   
  (:documentation "Entity class"))
  
(defun make-entity (tag)
  (let* ((new-entity (make-instance '<entity>
				    :tag tag))
	 (id (entity-id new-entity)))
    (setf (entity-id new-entity) (+ id 1))
    new-entity))


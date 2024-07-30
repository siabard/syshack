;;;; entity 

(defclass entity ()
  ((id :accessor entity-id 
       :initarg :id
       :allocation :class))
  (:documentation "Entity class"))
   

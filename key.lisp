(in-package #:syshack)


;; KEY related function
;; TODO: should refactoring to class or struct

(defclass <key-input> () 
  ((pressed :accessor key-input-pressed
	    :initarg :pressed)
   (released :accessor key-input-release
	     :initarg :released)
   (held :accessor key-input-held
	 :initarg :held)))


(defun make-key-input ()
  (make-instance '<key-input>
		 :pressed (make-hash-table :test 'equal)
		 :released (make-hash-table :test 'equal)
		 :held (make-hash-table :test 'equal)))

;;(defvar *pressed-key* (make-hash-table :test #'equal))
;;(defvar *released-key* (make-hash-table :test #'equal))
;;(defvar *held-keys* (make-hash-table :test #'equal))

(defgeneric init-keys (new-key-input)
  (:documentation "Key input initialized"))

(defmethod init-keys ((new-key-input <key-input>))
  (setf (slot-value new-key-input 'pressed) (make-hash-table :test 'equal))
  (setf (slot-value new-key-input 'released) (make-hash-table :test 'equal))
  (setf (slot-value new-key-input 'held) (make-hash-table :test 'equal)))

(defgeneric clear-keys (keys)
  (:documentation "clear released, pressed info"))

(defmethod clear-keys ((keys <key-input>))
  (clrhash (slot-value keys 'pressed))
  (clrhash (slot-value keys 'released)))

(defgeneric keyup-event (keys scancode)
  (:documentation "Catch key up event"))

(defmethod keyup-event ((keys <key-input>) scancode)
  (let ((released (slot-value keys 'released))
	(held (slot-value keys 'held)))
    (setf (gethash scancode released) t)
    (setf (gethash scancode held) nil)))

(defgeneric keydown-event (keys scancode)
  (:documentation "Catch key down event"))

(defmethod keydown-event ((keys <key-input>) scancode)
  (let ((pressed (slot-value keys 'pressed))
	(held (slot-value keys 'held)))
    (setf (gethash scancode pressed) t)
    (setf (gethash scancode held) t)))


(defgeneric key-pressed-p (keys scancode)
  (:documentation "determine key for scancode is pressed"))

(defmethod key-pressed-p ((keys <key-input>) scancode)
  (let ((pressed (slot-value keys 'pressed)))
    (gethash scancode pressed nil)))

(defgeneric key-released-p (keys scancode)
  (:documentation "determine key for scancode is released"))

(defmethod key-released-p ((keys <key-input>) scancode)
  (let ((released (slot-value keys 'released)))
    (gethash scancode released nil)))


(defgeneric key-held-p (keys scancode)
  (:documentation "determine key for held"))

(defmethod key-held-p ((keys <key-input>) scancode)
  (let ((held (slot-value keys 'held)))
    (gethash scancode held nil)))


(defgeneric quit-keys (keys)
  (:documentation "clear all state of keys"))

(defmethod quit-keys (keys)
  (clrhash (slot-value keys 'held))
  (clrhash (slot-value keys 'pressed))
  (clrhash (slot-value keys 'released)))


;; //END OF KEY related function


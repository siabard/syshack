(in-package #:syshack)

;; gui
(defclass <gui> () 
  ((x :initarg :x
      :accessor gui-x
      :initform 0)
   (y :initarg :y
      :accessor gui-y
      :initform 0)
   (w :initarg :w
      :accessor gui-w
      :initform 0)
   (h :initarg :h
      :accessor gui-h
      :initform 0)))

(defun make-gui (&key (x 0) (y 0) (w 0) (h 0))
  (make-instance '<gui> 
		 :x x
		 :y y
		 :w w
		 :h h))

(defgeneric update-gui (gui dt)
  (:documentation "update gui with when elapsed dt"))

(defgeneric render-gui (gui renderer)
  (:documentation "rendering gui"))

(defgeneric render-width (gui)
  (:documentation "rendering width"))

(defgeneric render-height (gui)
  (:documentation "rendering height"))

(defgeneric set-pos-gui (gui new-x new-y)
  (:documentation "set x, y position"))

;; 키 입력처리
(defgeneric process-key-event (gui scancode)
  (:documentation "key event 처리"))

(defmethod set-pos-gui (gui new-x new-y)
  (setf (gui-x gui) new-x)
  (setf (gui-y gui) new-y))


;; 입력 (키, 마우스 처리)

(defgeneric handle-input-gui (gui &key keyboard mouse)
  (:documentation "키보드 / 마우스 처리"))


(defmethod handle-input-gui (gui &key keyboard mouse)
  (let ((key-list (mapcar #'sdl2:scancode-key-to-value '(:scancode-up :scancode-down :scancode-left :scancode-right))))
    (dolist (scancode key-list)
      (when (key-pressed-p keyboard scancode)
	(process-key-event gui scancode)))))

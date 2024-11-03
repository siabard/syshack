(in-package #:syshack)


(defclass <dialog-window> (gui) 
  ((panel :initarg :panel
	  :accessor panel)
   (index :initarg :index
	  :accessor index)
   (title :initarg :title
	  :accessor title)
   (texts-length :initarg :texts-length
		 :accessor texts-length)
   (texts :initarg :texts
	  :accessor texts)
   (avatar :initarg :avatar
	   :initform nil
	   :accessor avatar)
   (choice :initarg :choice
	   :accessor choice)))

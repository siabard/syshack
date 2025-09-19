;; GUI 요소 (panel / dialog를 관리하는 클래스)
(in-package #:syshack)

(defclass <gui-manager> ()
  ((panels :initarg :panels
	   :accessor panels)
   (dialogs :initarg :dialogs
	    :accessor dialogs)))

(defun make-gui-manager ()
  (let ((panels (make-hash-table :test 'equal))
	(dialogs (make-hash-table :test 'equal)))
    (make-instance '<gui-manager>
		   :panels panels
		   :dialogs dialogs)))


;; 다이얼로그 추가하기

(defun gui/add-dialog (guim did x y w h panel title texts avatar choices)
  (let* ((dialog (make-dialog-window x y w h panel title texts avatar choices))
	 (dialogs (dialogs guim)))
    (setf (gethash did dialogs) dialog)))


;; 특정 다이얼로그 설정 변경 


(defun gui/set-dialog (guim did dialog)
  (let* ((dialogs (dialogs guim)))
    (setf (gethash did dialogs) dialog)))


;; 다이얼로그 클린업 

(defun gui/cleanup-dialog (guim)
  (let* ((dialogs (dialogs guim))
	 (panels (panels guim)))
    (clrhash dialogs)
    (clrhash panels)))


;; 다이얼로그 렌더링 
(defun gui/render-dialog (guim renderer korean-bitmap-font ascii-bitmap-font)
  (let* ((dialogs (dialogs guim)))
    (loop for v being the hash-value in dialogs 
	  do (dialog-window/render v renderer
				   :korean-bitmap-font korean-bitmap-font
				   :ascii-bitmap-font ascii-bitmap-font))))

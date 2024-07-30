;;;; Asset 을 관리하는 클래스
(in-package #:syshack)

(defclass <asset-manager> ()
  ((textures :accessor asset-manager-textures
	     :initarg :textures )
   (renderer :accessor asset-manager-renderer
	     :initarg :renderer))
  (:documentation "Asset 관리체계"))


;;;; 생성하기
(defun make-asset-manager (renderer)
  (let ((textures (make-hash-table :test 'equal)))
    (make-instance '<asset-manager>
		   :textures textures
		   :renderer renderer)))

;;;; 정리하기 
(defgeneric asset-manager/cleanup (asset-manager)
  (:documentation "clean up every assets"))


(defmethod asset-manager/cleanup (asset-manager)
  (let ((textures (asset-manager-textures asset-manager)))
    (loop for v being the hash-value in textures
	  do (safe-delete-texture v))))


;;;; texture 추가하기 (renderer 필요)
(defgeneric asset-manager/add-texture (asset-manager name path)
  (:documentation "Add new texture to asset manager's textures"))

(defmethod asset-manager/add-texture (asset-manager name path)
  (let* ((textures (asset-manager-textures asset-manager))
	 (renderer (asset-manager-renderer asset-manager))
	 (texture (load-texture renderer path)))
    (setf (gethash name textures) texture)))



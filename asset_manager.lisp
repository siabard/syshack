;;;; Asset 을 관리하는 클래스
(in-package #:syshack)

(defclass <asset-manager> ()
  ((textures :accessor asset-manager-textures
	     :initarg :textures)
   (fonts :accessor asset-manager-fonts
	  :initarg :fonts)
   (renderer :accessor asset-manager-renderer
	     :initarg :renderer))
  (:documentation "Asset 관리체계"))


;;;; 생성하기
(defun make-asset-manager (renderer)
  (let ((textures (make-hash-table :test 'equal))
	(fonts (make-hash-table :test 'equal)))
    (make-instance '<asset-manager>
		   :textures textures
		   :fonts fonts
		   :renderer renderer)))

;;; 애셋 가져오기
(defgeneric asset-manager/get-texture (asset-manager name)
  (:documentation "Get texture for given name"))

(defmethod asset-manager/get-texture (asset-manager name)
  (let ((textures (asset-manager-textures asset-manager)))
    (gethash name textures)))

(defgeneric asset-manager/get-font (asset-manager name)
  (:documentation "Get font for given name"))

(defmethod asset-manager/get-font (asset-manager name)
  (let ((fonts (asset-manager-fonts asset-manager)))
    (gethash name fonts)))

;;;; 정리하기 
(defgeneric asset-manager/cleanup (asset-manager)
  (:documentation "clean up every assets"))


(defmethod asset-manager/cleanup (asset-manager)
  (let ((textures (asset-manager-textures asset-manager))
	(fonts (asset-manager-fonts asset-manager)))
    (loop for v being the hash-value in textures
	  do (safe-delete-texture v))
    (clrhash textures)
    (clrhash fonts)))


;;;; texture 추가하기 (renderer 필요)
(defgeneric asset-manager/add-texture (asset-manager name path)
  (:documentation "Add new texture to asset manager's textures"))

(defmethod asset-manager/add-texture (asset-manager name path)
  (let* ((textures (asset-manager-textures asset-manager))
	 (renderer (asset-manager-renderer asset-manager))
	 (texture (load-texture renderer path)))
    (setf (gethash name textures) texture)))


;;;; font 추가 (imago 이용)
(defgeneric asset-manager/add-font (asset-manager name path)
  (:documentation "Add new bitmap font to asset manager's fonts"))

(defmethod asset-manager/add-font (asset-manager name path)
  (let* ((fonts (asset-manager-fonts asset-manager)))
    (setf (gethash name fonts) (imago:read-image path))))	 

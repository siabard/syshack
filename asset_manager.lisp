;;;; Asset 을 관리하는 클래스
(in-package #:syshack)

(defclass <asset-manager> ()
  ((textures :accessor asset-manager-textures
	     :initarg :textures)
   (map-textures :accessor asset-manager-map-textures
		 :initarg :map-textures)
   (atlases :accessor asset-manager-atlas
	    :initarg :atlases)
   (fonts :accessor asset-manager-fonts
	  :initarg :fonts)
   (gamemaps :accessor asset-manager-gamemaps
	     :initarg :gamemaps)
   (animations :accessor asset-manager-animations
	       :initarg :animations)
   (renderer :accessor asset-manager-renderer
	     :initarg :renderer))
  (:documentation "Asset 관리체계"))


;;;; 생성하기
(defun make-asset-manager (renderer)
  (let ((textures (make-hash-table :test 'equal))
	(map-textures (make-hash-table :test 'equal))
	(gamemaps (make-hash-table :test 'equal))
	(atlases (make-hash-table :test 'equal))
	(animations (make-hash-table :test 'equal))
	(fonts (make-hash-table :test 'equal)))
    (make-instance '<asset-manager>
		   :textures textures
		   :map-textures map-textures
		   :atlases atlases
		   :gamemaps gamemaps
		   :animations animations
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

;;;; 게임 맵 추가하기
(defgeneric asset-manager/get-gamemap (asset-manager name)
  (:documentation "Game tiled map info"))

(defmethod asset-manager/get-gamemap (asset-manager name)
  (let ((gamemaps (asset-manager-fonts asset-manager)))
    (gethash name gamemaps)))

;;;; 정리하기 
(defgeneric asset-manager/cleanup (asset-manager)
  (:documentation "clean up every assets"))


(defmethod asset-manager/cleanup (asset-manager)
  (let ((textures (asset-manager-textures asset-manager))
	(map-textures (asset-manager-textures asset-manager))
	(atlases (asset-manager-atlas asset-manager))
	(gamemaps (asset-manager-gamemaps asset-manager))
	(animations (asset-manager-animations asset-manager))
	(fonts (asset-manager-fonts asset-manager)))
    (loop for v being the hash-value in textures
	  do (release-texture v))
    (loop for v being the hash-value in map-textures
	  do (release-texture v))
    (clrhash textures)
    (clrhash animations)
    (clrhash gamemaps)
    (clrhash atlases)
    (clrhash fonts)))


;;;; texture 추가하기 (renderer 필요)
(defgeneric asset-manager/add-texture (asset-manager name path tile-width tile-height)
  (:documentation "Add new texture to asset manager's textures"))

(defmethod asset-manager/add-texture (asset-manager name path tile-width tile-height)
  (let* ((textures (asset-manager-textures asset-manager))
	 (renderer (asset-manager-renderer asset-manager))
	 (texture (make-texture renderer path tile-width tile-height)))
    (setf (gethash name textures) texture)))



;;; map-texture 추가하기 (renderer 필요)
(defgeneric asset-manager/add-map-texture (asset-manager name path tile-width tile-height)
  (:documentation "Add new map texture to asset manager's map-textures"))

(defmethod asset-manager/add-map-texture (asset-manager name path tile-width tile-height)
  (let* ((textures (asset-manager-map-textures asset-manager))
	 (renderer (asset-manager-renderer asset-manager))
	 (texture (make-texture renderer path tile-width tile-height)))
    (setf (gethash name textures) texture)))

;;;; font 추가 (imago 이용)
(defgeneric asset-manager/add-font (asset-manager name path)
  (:documentation "Add new bitmap font to asset manager's fonts"))

(defmethod asset-manager/add-font (asset-manager name path)
  (let* ((fonts (asset-manager-fonts asset-manager)))
    (setf (gethash name fonts) (imago:read-image path))))	 

;;;; gamemap 추가 
(defgeneric asset-manager/add-gamemap (asset-manager name path)
  (:documentation "게임맵을 Asset manager에 추가함"))


;; game map 이 추가되면서 map 용 texture도 등록을 하게 됨
;; texture에는 atlas 가 생성될 것임
(defmethod asset-manager/add-gamemap (asset-manager name path)
  (let* ((gamemaps (asset-manager-gamemaps asset-manager)))
    (setf (gethash name gamemaps) (make-tiled-map asset-manager path))))

;;;; TODO Animation 추가
(defgeneric asset-manager/add-animation (asset-manager name animation)
  (:documentation "애니메이션을 ㅁsset Manager에 추가함"))

(defmethod asset-manager/add-animation ((asset-manager <asset-manager> name animation))
  (let* ((animations (assett-manager-animations asset-manager)))
    (setf (gethash name animations) animation)))

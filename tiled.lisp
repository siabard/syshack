(in-package #:syshack)

;;;; tiled를 이용하여 tile 정보 구성하기 

;;;; tileset 은 tileset 이미지에 대한 클래스
;;;; atlas 를 만들어줄 수 있어야함.
;;;; texture 때문에 renderer 에 종속적임.

(defclass <tileset-data> () 
  ((first-gid :accessor tileset-data-first-gid
	      :initarg :first-gid)
   (texture-name :accessor tileset-data-texture-name
		 :initarg :texture-name)
   (columns :accessor tileset-data-columns
	    :initarg :columns)))

;; load tilemap
;; return layers and tilesets
(defun load-tiled-map (path-to-map-file)
  "Return tiled map structure. Return NIL when loading is failed"
  (let ((map-path (truename (pathname path-to-map-file))))
    (when (probe-file map-path)
      (cl-tiled:load-map map-path))))


;; tilemap 클래스 

(defclass <tiled-map> ()
  ((layers :accessor tiled-map-layers
	   :initarg :layers)
   (tilesets :accessor tiled-map-tilesets
	     :initarg :tilesets)
   (altas :accessor tiled-map-atlas 
	  :initarg :atlas)
   (width :accessor tiled-map-width
	  :initarg :width)
   (height :accessor tiled-map-height
	   :initarg :height)
   (tile-width :accessor tiled-map-tile-width 
	       :initarg :tile-width)
   (tile-height :accessor tiled-map-tile-height
		:initarg :tile-height)
   (triggers :accessor tiled-map-triggers
	     :initarg :triggers)
   (cam-x :accessor tiled-map-cam-x
	  :initarg :cam-x)
   (cam-y :accessor tiled-map-cam-y
	  :initarg :cam-y)))
   

;;;; cl-tiled:tileset 정보를 통해서 asset-manager의 map-textures 를 셋업하고
;;;; 인스탄스도 만들기 
(defun make-tileset-data (asset-manager tileset)
  (let* ((texture-name (cl-tiled:tileset-name tileset))
	 (first-gid (cl-tiled:tileset-first-gid tileset))
	 (columns (cl-tiled:tileset-columns tileset))
	 (path (cl-tiled:image-source (cl-tiled:tileset-image tileset)))
	 (tile-width (cl-tiled:tileset-tile-width tileset))
	 (tile-height (cl-tiled:tileset-tile-height tileset)))
    (asset-manager/add-map-texture asset-manager texture-name (truename path) tile-width tile-height)
    (make-instance '<tileset-data>
		   :first-gid first-gid
		   :texture-name texture-name
		   :columns columns)))

(defun make-tiled-map (asset-manager path)
  (let ((map-data (load-tiled-map path)))
    (cond (map-data 
	   (let* ((layers (cl-tiled:map-layers map-data))
		  (tilesets (cl-tiled:map-tilesets map-data))
		  (width (cl-tiled:map-width map-data))
		  (height (cl-tiled:map-height map-data))
		  (tile-width (cl-tiled:map-tile-width map-data))
		  (tile-height (cl-tiled:map-tile-height map-data))
		  (tileset-data (mapcar #'(lambda (tdata) (make-tileset-data asset-manager tdata)) tilesets)))
	     (make-instance '<tiled-map>
			    :layers layers
			    :tilesets tileset-data
			    :tile-width tile-width
			    :tile-height tile-height
			    :width width
			    :height height))))))
		       


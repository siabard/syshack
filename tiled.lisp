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
		  (sorted-tilesets
		    (sort tilesets
			  #'>
			  :key #'cl-tiled:tileset-first-gid))
		  (tileset-data 
		    (mapcar #'(lambda (tdata) 
				(make-tileset-data asset-manager tdata))
			    sorted-tilesets)))
	     (make-instance '<tiled-map>
			    :layers layers
			    :tilesets tileset-data
			    :tile-width tile-width
			    :tile-height tile-height
			    :width width
			    :height height))))))


;; tile id 에 따라 tileset 을 돌려주기 
;; tile atlas 는 각 texture의  atlas를 0부터 돌려준다.
;; 그러니 tile id 가 0 이 아닌 경우라면 해당 tileset 의 first-gid 를 빼준 값이
;; atlas 값이 된다. 
;; tiledmap 객체의 tilesets 데이터는 first-gid 의 내림차순으로 정렬되어있다.



;; asset manager / map-texture 의 키 값을 알아냈다.
;; 그렇다면 이제 텍스쳐와 (x y w h)값을 알아낼 수 있음..
(defun map-tile-info-map-texture (map tileid) 
  (let* ((tilesets (tiled-map-tilesets map))
	 (tilesets-contain-tileid 
	   (find-if #'(lambda (tileset)
			(let* ((first-gid (tileset-data-first-gid tileset)))
			  (>= tileid first-gid)))
		    tilesets)))
    (values (- tileid (tileset-data-first-gid tilesets-contain-tileid))
	    (tileset-data-texture-name tilesets-contain-tileid))))

(defun get-map-texture-and-atlas (asset-manager index texture-name)
  (let* ((map-texture (gethash texture-name (asset-manager-map-textures asset-manager)))
	 (texture (ctexture-texture map-texture))
	 (atlas (ctexture-atlas map-texture)))
    (values texture (aref atlas index))))


(defparameter *map-table* nil)
(defparameter *current-map* nil)

(defun set-map-table-from-game (game)
  (setf *map-table* (get-map-from-game game)))

(defun set-current-map (map-table map-key)
  (setf *current-map* (gethash map-key map-table)))



;; (set-map-table-from-game *game*)

;; (set-current-map *map-table* "level1")

;; (get-atlas-info *current-map* 1)

;; (get-map-texture-and-atals (game-asset-manager *game*)  0 "tilesheet")

;; (loop for v in (cl-tiled:layer-cells (car (cl-tiled:map-layers *tilemap*))) do 
;;   (format t "~A~%" (cl-tiled:tile-id  (cl-tiled:cell-tile  v))))


;; (format t "~A~%" (cl-tiled:map-tilesets *tilemap*))


;; (format t "~A~%"
;; 	(cl-tiled:tileset-first-gid (cadr (cl-tiled:map-tilesets *tilemap*))))

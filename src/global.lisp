(in-package #:syshack)

(defparameter *map-table* nil)
(defparameter *current-map* nil)
(defparameter *current-layers* nil)
(defparameter *current-scene* nil)
(defparameter *current-camera* nil)
(defparameter *current-game* nil)

(defun set-map-table-from-game (game)
  (setf *map-table* (get-map-from-game game)))

(defun set-current-map (map-table map-key)
  (setf *current-map* (gethash map-key map-table)))


(defun set-current-layer (map)
  (setf *current-layers* (tiled-map-layers map)))


(defun set-current-scene (game)
  (let* ((current-scene-name (game-current-scene game))
	 (scenes (game-scenes game))
	 (scene (gethash current-scene-name scenes)))
    (setf *current-scene* scene)))

(defun set-current-camera (scene)
  (setf *current-camera* (scene-zelda-camera scene)))

;; (set-map-table-from-game *game*)

;; (set-current-map *map-table* "level1")

;; (get-atlas-info *current-map* 1)

;; (get-map-texture-and-atals (game-asset-manager *game*)  0 "tilesheet")

;; (loop for v in (cl-tiled:layer-cells (car (cl-tiled:map-layers *tilemap*))) do 
;;   (format t "~A~%" (cl-tiled:tile-id  (cl-tiled:cell-tile  v))))


;; (format t "~A~%" (cl-tiled:map-tilesets *tilemap*))


;; (format t "~A~%"
;; 	(cl-tiled:tileset-first-gid (cadr (cl-tiled:map-tilesets *tilemap*))))


;; layer
;; (car *current-layers*)

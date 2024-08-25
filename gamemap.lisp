(in-package #:syshack)

;; *game* 으로부터 map 인스탄스 가져오기

(defun get-map-from-game (game)
  (let* ((current-scene-name (game-current-scene game))
	 (scenes (game-scenes game))
	 (scene (gethash current-scene-name scenes))
	 (game-map (scene-zelda-gamemap scene)))
    game-map))


;; x,y 값을 tile의 좌표로 변환한다.
(defun pos-to-tile (x y tile-width tile-height)
  (values (floor x tile-width) (floor y tile-height)))

;; camera 의 좌표를 토대로 tile의 x y w h 로 변환한다.
;; tile x y w h 는 타일의 갯수에 관한 것으로
;; 픽셀 사이즈가 아니다.
(defun camera-tile (camera tile-width tile-height)
  (let* ((x (camera-x camera))
	 (y (camera-y camera))
	 (w (camera-w camera))
	 (h (camera-h camera)))
    (values (floor x tile-width)
	    (floor y tile-height)
	    (ceiling w tile-width)
	    (ceiling h tile-height))))


(defun tile-is-inside-of (cl-tile x y w h)
  (let* ((column (cl-tiled:cell-column cl-tile))
	 (row (cl-tiled:cell-row cl-tile)))
    (and (<= x column)
	 (>= (+ x (- w 1)) column)
	 (<= y row)
	 (>= (+ y (- h 1)) row))))
	     

;; 각 map layer 를 출력한다.
;; map layer 는 camera 값을 받아 x y w h 를 통해 출력가능한
;; cell 을  결정해야한다.
;; cells 의 각 항목에는 COLUMN, ROW 항목이 있으며
;; 이 항목을 통해 출력이 가능한 영역인지 확인해야한다.
(defun clip-layer-with-camera (camera layer)
  (let* ((map (cl-tiled:layer-map layer))
	 (tile-width (cl-tiled:map-tile-width map))
	 (tile-height (cl-tiled:map-tile-height map))
	 (cells (cl-tiled:layer-cells layer)))
    
    (multiple-value-bind (cx cy cw ch)
	(camera-tile camera tile-width tile-height)
      (remove-if-not #'(lambda (cell) 
			 (tile-is-inside-of cell cx cy cw ch))
		     cells))))
			       

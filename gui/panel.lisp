(in-package #:syshack)

;; panel

;; panel은 9개의 tile로 이루어진 집합으로
;; 네 모서리(Top Left, Top Right, Bottom Left, Bottom Right)는 1:1로 출력되고
;; 네 변은 Panel의 가로/세로에서 tile의 가로/세로를 두 배한 만큼 뺀 크기를 가진다.
;; 해당 TILE을 노출할 때 src_rect -> dest_rect 를 변환하는 작업을 거치면 된다.

(defstruct panel-struct texture)

(defun make-panel (texture)
  (make-panel-struct :texture texture))

(defun panel/render (panel renderer x y w h)
  (let* ((panel-texture     (panel-struct-texture panel))
	 (panel-atlas       (ctexture-atlas panel-texture))
	 (texture           (ctexture-texture panel-texture))
	 (panel-top-left    (aref panel-atlas 0))
	 (panel-top-mid     (aref panel-atlas 1))
	 (panel-top-right   (aref panel-atlas 2))
	 (panel-mid-left    (aref panel-atlas 3))
	 (panel-mid-mid     (aref panel-atlas 4))
	 (panel-mid-right   (aref panel-atlas 5))
	 (panel-bot-left    (aref panel-atlas 6))
	 (panel-bot-mid     (aref panel-atlas 7))
	 (panel-bot-right   (aref panel-atlas 8))
	 (panel-width       (ctexture-tile-width panel-texture))
	 (panel-height      (ctexture-tile-height panel-texture))
	 (panel-width-span  (- w (* 2 panel-width)))
	 (panel-height-span (- h (* 2 panel-height))))
    (when (and (> panel-width-span 0) (> panel-height-span 0))
      (sdl2:render-copy-ex 
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-top-left) 
       :dest-rect (sdl2:make-rect x y panel-width panel-height))
      (sdl2:render-copy-ex
       renderer
       texture 
       :source-rect (list-to-sdl2-rect panel-top-mid)
       :dest-rect (sdl2:make-rect (+  x panel-width) y panel-width-span panel-height))
      (sdl2:render-copy-ex 
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-top-right)
       :dest-rect (sdl2:make-rect (+  x panel-width panel-width-span) y panel-width panel-height))
      (sdl2:render-copy-ex 
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-mid-left)
       :dest-rect (sdl2:make-rect x (+  y panel-height) panel-width  panel-height-span))
      (sdl2:render-copy-ex
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-mid-mid)
       :dest-rect (sdl2:make-rect (+  x panel-width) (+  y panel-height) panel-width-span panel-height-span))
      (sdl2:render-copy-ex 
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-mid-right)
       :dest-rect (sdl2:make-rect (+  x panel-width panel-width-span) (+ y panel-height) panel-width panel-height-span))
      (sdl2:render-copy-ex 
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-bot-left)
       :dest-rect (sdl2:make-rect x (+  y panel-height panel-height-span) panel-width panel-height))
      (sdl2:render-copy-ex 
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-bot-mid)
       :dest-rect (sdl2:make-rect (+  x panel-width)  (+  y panel-height panel-height-span) panel-width-span panel-height))
      (sdl2:render-copy-ex 
       renderer 
       texture 
       :source-rect (list-to-sdl2-rect panel-bot-right)
       :dest-rect (sdl2:make-rect (+  x panel-width panel-width-span) (+ y panel-height panel-height-span) panel-width panel-height)))))


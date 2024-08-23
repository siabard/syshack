(in-package #:syshack)

;; *game* 으로부터 map 인스탄스 가져오기

(defun get-map-from-game (game)
  (let* ((current-scene-name (game-current-scene game))
	 (scenes (game-scenes game))
	 (scene (gethash current-scene-name scenes))
	 (game-map (scene-zelda-gamemap scene)))
    game-map))




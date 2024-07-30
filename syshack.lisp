;;;; syshack.lisp

(in-package #:syshack)

;; main routine

;; SDL2 초기화

(defun main ()
  (let ((width 640)
	(height 480))
    (sdl2:with-init (:video :audio)
      (sdl2:with-window (win :title "Syshack" :flags '(:shown) :w width :h height)
	(sdl2:with-renderer (renderer win :flags '(:accelerated :targettexture :presentvsync))
	  (let ((game (make-game "Game" win renderer)))
	    (game/loop game)))))))

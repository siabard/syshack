(in-package #:syshack)

;; 도움함수 모음 (CFFI 등 포함)

;; c로 메모리 접근을 하는 함수
(cffi:defcfun memset :pointer
  (ptr :pointer)
  (val :int)
  (size :int))



;;;; list to rect
;;;; (x y w h) 형태를 sdl:rect로 전환 
(defun list-to-sdl2-rect (lst)
  (let ((x (car lst))
	(y (cadr lst))
	(w (caddr lst))
	(h (cadddr lst)))
    (sdl2:make-rect x y w h)))


;;;; sx, sy 배율로 확대하기
;;; rect logical->physical
(defun rect/logical->physical (x y w h &key (sx 1) (sy 1))
  (values (* x sx) (* y sy) (* w sx) (* h sy)))

;;; sx, sy 배율로 축소시키기
;; rect physical->logical
(defun rect/physical->logical (x y w h &key (sx 1) (sy 1))
  (values (floor x sx) (floor y sy) (floor w sx) (floor h sy)))

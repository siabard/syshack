(in-package #:syshack)

;; vec2 는 다음과 같다.
;; (make-vec2 x y)

(defstruct vec2
  x
  y)


(defun vec2-delta (v1 v2)
  (let ((delta-x (abs (- (vec2-x v1) (vec2-x v2))))
	(delta-y (abs (- (vec2-y v1) (vec2-y v2)))))
    (make-vec2 :x delta-x :y delta-y)))

;; rectangle 은 다음과 같다.
;; (make-rectangle x y w h)
(defstruct rectangle
  x y w h)


;; rectangle의 center를 구한다.
(defun rectangle-center (r)
  (let* ((x (rectangle-x r))
	 (y (rectangle-y r))
	 (w (rectangle-w r))
	 (h (rectangle-h r)))
    (values (+ x (/ w 2))
	    (+ y (/ h 2)))))


;; 한 rectangle과 또 다른 rectangle이 겹쳐진 위치를 구한다.
;; 겹쳐진 위치는 (x y) 같은 형식으로 구해진다.

(defun overlap-amount (r1 r2)
  (let* ((c1 (rectangle-center r1))
	 (c2 (rectangle-center r2))
	 (delta (vec2-delta c1 c2))
	 (ox (- (+ (/ (rectangle-w r1) 2)
		   (/ (rectangle-w r2) 2))
		(vec2-x delta)))
	 (oy (- (+ (/ (rectangle-h r1) 2)
		   (/ (rectangle-h r2) 2))
		(vec2-y delta))))
    (make-vec2 :x ox :y oy)))


;; vec2인 pos을 중점으로 둔 w x h 사각형
(defun get-bound-rect (pos w h)
  (let ((left (- (vec2-x pos) (/ w 2)))
	(top  (- (vec2-y pos) (/ h 2))))
    (make-rectangle :x left
		    :y top
		    :w w
		    :h h)))

;;

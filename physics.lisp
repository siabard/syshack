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
;; rectangle -> vec2
(defun rectangle-center (r)
  (let* ((x (rectangle-x r))
	 (y (rectangle-y r))
	 (w (rectangle-w r))
	 (h (rectangle-h r)))
    (make-vec2 :x (+ x (/ w 2))
	       :y (+ y (/ h 2)))))


;; r1의 [x, x+w] 가 r2의 [x, x+w] 에 포함되는지 확인한다.

(defun rectangle-x-contains? (r1 r2)
  (let ((r1-l (rectangle-x r1))
	(r1-r (+ (rectangle-x r1) (rectangle-w r1)))
	(r2-l (rectangle-x r2))
	(r2-r (+ (rectangle-x r2) (rectangle-w r2))))
    (and (>= r1-l r2-l)
	 (<= r1-r r2-r))))
  
;; r1의 [y, y+h]가 r2의 [yy, y+h]에 포함되는지 확인한다.
(defun rectangle-y-contains? (r1 r2)
  (let ((r1-t (rectangle-y r1))
	(r1-b (+ (rectangle-y r1) (rectangle-h r1)))
	(r2-t (rectangle-y r2))
	(r2-b (+ (rectangle-y r2) (rectangle-h r2))))
    (and (>= r1-t r2-t)
	 (<= r1-b r2-b))))

;; r1이 r2에 포함되는지 확인한다. 
(defun rectangle-contains? (r1 r2)
  (and (rectangle-x-contains? r1 r2)
       (rectangle-y-contains? r1 r2)))

;; 한 rectangle과 또 다른 rectangle이 겹쳐진 크기를 구한다.
;; 겹쳐진 크기는 (w h) 같은 형식으로 구해진다.
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
    (make-vec2 
     :x (cond ((rectangle-x-contains? r1 r2)
	       (rectangle-w r1))
	      ((rectangle-x-contains? r2 r1)
	       (rectangle-w r2))
	      (t ox))
     :y (cond ((rectangle-y-contains? r1 r2)
	       (rectangle-h r1))
	      ((rectangle-y-contains? r2 r1)
	       (rectangle-h r2))
	      (t oy)))))

;; (defparameter *r1* (make-rectangle :x 0 :y 0 :w 30 :h 30))
;; (defparameter *r2* (make-rectangle :x 0 :y 0 :w 100 :h 100))

;; vec2인 pos을 중점으로 둔 w x h 사각형
(defun get-bound-rect (pos w h)
  (let ((left (- (vec2-x pos) (/ w 2)))
	(top  (- (vec2-y pos) (/ h 2))))
    (make-rectangle :x left
		    :y top
		    :w w
		    :h h)))

;; entity rectangle과 camera rectangle 을 비교하여
;; entity rectangle의 source rect 범위를 정한다.
(defun clip-rect-src (entity-rect camera-rect)
  (cond ((rectangle-contains? entity-rect camera-rect)
	 (make-rectangle :x 0 
			 :y 0
			 :w (rectangle-w entity-rect)
			 :h (rectangle-h entity-rect)))
	((rectangle-contains? camera-rect entity-rect)
	 (make-rectangle 
	  :x (- (rectangle-x camera-rect)
		(rectangle-x entity-rect))
	  :y (- (rectangle-y camera-rect)
		(rectangle-y entity-rect))
	  :w (rectangle-w camera-rect)
	  :h (rectangle-h camera-rect)))
	(t
	 (let* ((overlapped (overlap-amount entity-rect camera-rect))
		(entity-x (rectangle-x entity-rect))
		(camera-x (rectangle-x camera-rect))
		(entity-y (rectangle-y entity-rect))
		(camera-y (rectangle-y camera-rect)))
	   ;; entity-rect.x 가 camera-rect.x 보다 작으면
	   ;; x -> cameraa-rect.x - entity-rect.x
	   ;; w -> overlapped.x
	   ;; entity-rect.x 가 camera-rect.x 보다 크면
	   ;; x -> 0 
	   ;; w -> overlapped.x
	   ;; entity-rect.y 가 camera-rect.y 보다 작으면
	   ;; y -> camera-rect.y - entity-rect.y
	   ;; h -> overlapped.h
	   ;; entity-rect.y 가 entity-rect.y 보다 크면
	   ;; y -> 0
	   ;; h -> overlapped.h
	   (make-rectangle
	    :x (cond ((< entity-x camera-x)
		      (- camera-x entity-x))
		     (t
		      0))
	    :w (vec2-x overlapped)
	    :y (cond ((< entity-y camera-y)
		      (- camera-y entity-y))
		     (t 0))
	    :h (vec2-y overlapped))))))



(defun list-to-rectangle (lst)
  (make-rectangle :x (car lst)
		  :y (cadr lst)
		  :w (caddr lst)
		  :h (cadddr lst)))



(defun rectangle-to-sdl-rect (rectangle)
  (let ((x (rectangle-x rectangle))
	(y (rectangle-y rectangle))
	(w (rectangle-w rectangle))
	(h (rectangle-h rectangle)))
    (sdl2:make-rect x y w h)))

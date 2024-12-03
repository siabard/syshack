(in-package #:syshack)



;;;; texts 는 한 화면에 나타낼 수 있는 문자열 단락을 리스트로 보관한다.
;;;; 예를 들어 
;;;; (chunk-text 
;;;;   '("우리는 미래를 만들어 나가야합니다." 
;;;;     "아주 긴 미래가 기다리고 있습니다." 
;;;;     "마법과 검의 환상 세계가...") 12 4)
;;;; 는 아래와 같이 한 줄이 12글자이고, 최대 4줄의 단락의 리스트를 만든다.
;;;; (("우리는 미래를 만들어" "나가야합니다." "아주 긴 미래가 기다리" "고 있습니다.")
;;;;  ("마법과 검의 환상 세계" "가..."))
(defclass <dialog-window> (<gui>) 
  ((panel :initarg :panel
	  :accessor dialog-window-panel)
   (index :initarg :index
	  :accessor dialog-window-index
	  :initform 0
	  :documentation "현재 표시해야할 texts 의 요소")
   (title :initarg :title
	  :accessor dialog-window-title)
   (texts-length :initarg :texts-length
		 :accessor dialog-window-texts-length)
   (texts :initarg :texts
	  :accessor dialog-window-texts)
   (avatar :initarg :avatar
	   :initform nil
	   :accessor dialog-window-avatar
	   :documentation "Sprite 타입을 사용한다.")
   (choice :initarg :choice
	   :accessor dialog-window-choice)))


(defun make-dialog-window (x y w h panel title texts avatar choice)
  (let* ((inner-width (- w 6))
	 (inner-height (- h 6))
	 (chopped-texts (chunk-text texts 
				   (floor inner-width 16)
				   (floor inner-height 16))))
    (make-instance '<dialog-window>
		   :x x
		   :y y
		   :w w
		   :h h
		   :panel panel
		   :index 0
		   :title title
		   :texts chopped-texts
		   :texts-length (floor inner-width 16)
		   :avatar avatar
		   :choice choice)))

;; 현재 index 를 하나 늘려 다음 위치로 이동  
;; 다음으로 이동하면 해당 인덱스를, 더이상 진행하지 못한다면 nil 읇 반환한다.
(defun dialog-window/next (dw)
  (let* ((current-index (dialog-window-index dw))
	 (next-index (+ current-index 1))
	 (dialog-length (length (dialog-window-texts dw))))
    (cond ((< next-index dialog-length)
	   (setf (dialog-window-index dw)
		 next-index)
	   next-index)
	  (t nil))))
	  

;; 렌더링
(defun dialog-window/render (dw renderer &key korean-bitmap-font ascii-bitmap-font)
  (let* ((panel (dialog-window-panel dw))
	 (x (gui-x dw))
	 (y (gui-y dw))
	 (w (gui-w dw))
	 (h (gui-h dw))
	 (texts (dialog-window-texts dw))
	 (current-texts (nth (dialog-window-index dw)
			     texts)))
    (panel/render panel renderer x y w h)
    (dolist (text current-texts)
      (draw-string renderer 
		   (+ x 3)
		   (+ y 3)
		   text
		   :korean-bitmap-font korean-bitmap-font
		   :ascii-bitmap-font ascii-bitmap-font)
      (incf y 16))))

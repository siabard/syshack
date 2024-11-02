;;;; bitmap_font.lisp
;; TODO scale-x scale-y 를 받아서 해당 값으로 확대/축소해서 렌더링할 것

(in-package #:syshack)

;; 파라미터 image 의 x, y 위치의 값을 읽어들인다.
;; 데이터의 값은 rgba 가 각각 1바이트씩 차지한 값이다.
(defun get-pixel-value (image x y)
  (multiple-value-bind (a r g b)
      (imago:color-argb (imago:image-pixel image x y))
    (color-to-byte r g b a)))

;; 이미지의 x, y, w, h 위치의 pixel을 pointer 영역으로
;; 전달한다.
(defun write-pixel-to-pointer (image pixels x y w h &optional chrom color)
  (dotimes (j h)
    (dotimes (i w)
      (let* ((p (+ i (* j w)))
	     (c (get-pixel-value image (+ x i) (+ j y)))
	     (color (cond ((null color) c)
			  (t color))))
	(unless (equal chrom c)
	  (setf (cffi:mem-ref pixels :uint32 (* 4 p)) color))))))


;; 지정한 char-code 에 맞추어 폰트를 가져온다.
;; 폰트 파일은 가로 16x세로 8 (16x8=128) 이다.
;; char-code 이 65인 A의 경우 4행 1열이므로
;; (3,0) 의 폰트 정보를 필요로한다.
(defun ascii-font-pos (charcode)
  (multiple-value-bind (q r) (floor charcode 16)
    (list q r)))

;; 아스키 폰트를 pixels 포인터 공간에 넣기
(defun write-font-to-pointer (image pixels charcode &optional chrom color)
  (multiple-value-bind (row col) (floor charcode 16)
    (let ((width 8)
	  (height 16))
      (write-pixel-to-pointer image
			      pixels
			      (* width col)
			      (* height row)
			      width
			      height
			      chrom
			      color))))

;; 한글 폰트를 pixels 포인터 공간에 넣기
(defun write-hangul-font-to-pointer (image pixels charcode &optional color)
  (let* ((jaso (han-jaso charcode))
	 (jaso-cho (car jaso))
	 (jaso-mid (cadr jaso))
	 (jaso-jong (caddr jaso))
	 (bul (han-bul jaso-cho jaso-mid jaso-jong))
	 (bul-cho (car bul))
	 (bul-mid (cadr bul))
	 (bul-jong (caddr bul))
	 (width 16)
	 (height 16))
    (write-pixel-to-pointer image
			    pixels
			    (* jaso-cho width)
			    (* bul-cho height)
			    width
			    height
			    0
			    color)
    (write-pixel-to-pointer image
			    pixels
			    (* jaso-mid width)
			    (+ (* 8 height) (* bul-mid height))
			    width
			    height
			    0
			    color)
    (when bul-jong
      (write-pixel-to-pointer image
			      pixels
			      (* jaso-jong width)
			      (+ (* 12 height) (* bul-jong height))
			      width
			      height
			      0
			      color))))


;;; 한글 여부 확인
(defun hangulp (charcode)
  (and (>= charcode #xac00)
       (<= charcode #xd7a3)))

;;; 한글 자소 확인
(defun hangul-jaso-p (charcode)
  (and (>= charcode #x3131)
       (<= charcode #x3163)))

;; 아스키 여부 확인
(defun asciip (charcode)
  (and (>= charcode #x0000)
       (<= charcode #x007f)))

;;; 일어 여부 확인
(defun kanap (charcode)
  (and (>= charcode #x3040)
       (<= charcode #x30ff)))

;;; 화살표 여부 확인
(defun arrowp (charcode)
  (and (>= charcode #x2190)
       (<= charcode #x2199)))

;;; 텍스트를 출력하는 기능
;;; 한글인 경우와 아스키에 따른 경우를 모두 계산할 것
(defun draw-string (renderer x y texts &key korean-bitmap-font ascii-bitmap-font (sx 1) (sy 1) (color nil))
  (let* ((charcodes (mapcar #'char-code (coerce texts 'list)))
	 (text-offset 0)
	 (texture (sdl2:create-texture renderer
				       sdl2:+pixelformat-rgba8888+
				       sdl2-ffi:+sdl-textureaccess-target+
				       16 16)))
    (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
    
    (dolist (charcode charcodes)
      (cond ((hangulp charcode)
	     (cffi:with-foreign-pointer (pixels (* 16 16 4))
	       (memset pixels 0 (* 16 16 4))
	       (write-hangul-font-to-pointer korean-bitmap-font
					     pixels
					     charcode
					     color)
	       (sdl2:update-texture texture
				    (sdl2:make-rect 0 0 16 16)
				    pixels
				    (* 16 4))
	       (sdl2:render-copy-ex renderer
				    texture
				    :source-rect (sdl2:make-rect 0 0 16 16)
				    :dest-rect (multiple-value-bind (zx zy zw zh)
						   (rect/logical->physical (+ x text-offset)
									   y
									   16
									   16
									   :sx sx
									   :sy sy)
						 (sdl2:make-rect zx zy zw zh))
				    :angle 0
				    :center (sdl2:make-point 0 0)
				    :flip nil)
	       (setf text-offset (+ 16 text-offset))))
	    ((asciip charcode)
	     (cffi:with-foreign-pointer (pixels (* 8 16 4))
	       (memset pixels 0 (* 8 16 4))
	       (write-font-to-pointer ascii-bitmap-font
				      pixels
				      charcode
				      color)
	       (sdl2:update-texture texture
				    (sdl2:make-rect 0 0 8 16)
				    pixels
				    (* 8 4))
	       (sdl2:render-copy-ex renderer
				    texture
				    :source-rect (sdl2:make-rect 0 0 8 16)
				    :dest-rect (multiple-value-bind (zx zy zw zh)
						   (rect/logical->physical (+ x text-offset)
									   y
									   8
									   16
									   :sx sx
									   :sy sy)
						 (sdl2:make-rect zx zy zw zh))
				    :angle 0
				    :center (sdl2:make-point 0 0)
				    :flip nil)
	       (setf text-offset (+ 8 text-offset))))))
	 (sdl2:destroy-texture texture)))


;; 한글을 출력해보기
(defun draw-hangul (renderer bitmap-font &optional color)
  (let ((texture (sdl2:create-texture renderer
				      sdl2:+pixelformat-rgba8888+
				      sdl2-ffi:+sdl-textureaccess-target+
				      16 16)))
    (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
    (cffi:with-foreign-pointer (pixels (* 16 16 4))
      (memset pixels 0 (* 16 16 4))
      (write-hangul-font-to-pointer bitmap-font
				    pixels
				    #xD55C
				    color)
      (sdl2:update-texture texture (sdl2:make-rect 0 0 16 16) pixels (* 16 4)))
    (sdl2:render-copy-ex renderer
			 texture
			 :source-rect (sdl2:make-rect 0 0 16 16)
			 :dest-rect (sdl2:make-rect 16 16 16 16)
			 :angle 0
			 :center (sdl2:make-point 0 0)
			 :flip nil)
    (sdl2:destroy-texture texture)))

(defun draw (renderer bitmap-font &optional color)
  (let ((texture (sdl2:create-texture renderer
				      sdl2:+pixelformat-rgba8888+
				      sdl2-ffi:+sdl-textureaccess-target+
				      8 16)))
    (sdl2:set-texture-blend-mode texture sdl2-ffi:+sdl-blendmode-blend+)
    (cffi:with-foreign-pointer (pixels (* 8 16 4))
      (memset pixels 0 (* 8 16 4))
      (write-font-to-pointer bitmap-font
			     pixels
			     (char-code #\C)
			     0
			     color)
      (sdl2:update-texture texture (sdl2:make-rect 0 0 8 16) pixels (* 8 4)))
    (sdl2:render-copy-ex renderer
			 texture
			 :source-rect (sdl2:make-rect 0 0 8 16)
			 :dest-rect (sdl2:make-rect 16 0 16 16)
			 :angle 0
			 :center (sdl2:make-point 0 0)
			 :flip nil)))


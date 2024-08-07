;;;; 한글 관련 초중종성 분리
(in-package #:syshack)

(defconstant +num-of-jong+ 28)
(defconstant +num-of-mid+ 21)

(defun han-jaso (charcode)
  (let* ((hancode (- charcode #xac00))
	 (jong (rem hancode +num-of-jong+)))
    (multiple-value-bind (cho mid) (floor 
				    (floor (- hancode jong)
					   +num-of-jong+)
				    +num-of-mid+)
      (list cho mid jong))))


(defun han-bul (cho mid jong)
  (cond ((equal 0 jong)
	 (list (cond ((or (equal mid 20)
			  (and (>= mid 0)
			       (<= mid 7)))
		      0)
		     ((or (equal mid 8)
			  (equal mid 12)
			  (equal mid 18))
		      1)
		     ((or (equal mid 13)
			  (equal mid 17))
		      2)
		     ((or (equal mid 19)
			  (and (>= mid 9)
			       (<= mid 11)))
		      3)
		     ((and (>= mid 14)
			   (<= mid 16))
		      4)
		     (t nil))
	       (cond ((or (equal 0 cho)
			  (equal 1 cho))
		      0)
		     ((and (>= cho 2)
			   (<= cho 18))
		      1)
		     (t nil))
	       nil))
	(t
	 (list (cond ((or (equal mid 20)
			  (and (>= mid 0)
			       (<= mid 7)))
		      5)
		     ((or (equal mid 8)
			  (equal mid 12)
			  (equal mid 13)
			  (equal mid 17)
			  (equal mid 18))
		      6)
		     ((or (equal mid 19)
			  (and (>= mid 9)
			       (<= mid 11))
			  (and (>= mid 14)
			       (<= mid 16)))
		      7)
		     (t nil))
	       (cond ((or (equal cho 0)
			  (equal cho 1))
		      2)
		     ((and (>= cho 2)
			   (<= cho 18))
		      3)
		     (t nil))
	       (cond ((or (equal mid 0)
			  (equal mid 2)
			  (equal mid 9))
		      0)
		     ((or (equal mid 4)
			  (equal mid 6)
			  (equal mid 11)
			  (equal mid 14)
			  (equal mid 16)
			  (equal mid 19)
			  (equal mid 20))
		      1)
		     ((or (equal mid 1)
			  (equal mid 3)
			  (equal mid 5)
			  (equal mid 7)
			  (equal mid 10)
			  (equal mid 15))
		      2)
		     ((or (equal mid 8)
			  (equal mid 12)
			  (equal mid 13)
			  (equal mid 17)
			  (equal mid 18))
		      3)
		     (t nil))))))

(in-package #:syshack)


(defun split-list (list len)
 ;; (split-list '(a b c d e f g) 3) => ((A B C) (D E F) (G))
 "Splits the list into sublists of length len. The last element might have fewer than len elements."
    (do* ((n 1 (1+ n))
          (l list (cdr l))
          (l1 nil)
          (res nil) )
         ((null l) (progn (when l1 (push (nreverse l1) res))(nreverse   res)))
        (push (car l) l1)
        (when (= n len)
            (push (nreverse l1) res)
            (setq l1 nil)
            (setq n 0) )))


;; 텍스트(스트링의 배열)을 일정 크기의 가로/세로에 맞는 크기로
;; 자른 텍스트 뭉치 배열로 만들기

(defun chunk-text (textlist width height)
  (let ((chunk '()))
    (dolist (text textlist)
      (push (mapcar (lambda (lst) (string-trim " " (coerce lst 'string)))
		    (split-list (coerce text 'list) width))
	    chunk))
    (split-list  (reduce #'append  (reverse  chunk)) height)))


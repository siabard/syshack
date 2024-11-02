(in-package #:syshack)

(defun color-to-byte (r g b a)
  (+ (ash r 24)
     (ash g 16)
     (ash b 8)
     a))

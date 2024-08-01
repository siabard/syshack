(in-package #:syshack)

;; 도움함수 모음 (CFFI 등 포함)

;; c로 메모리 접근을 하는 함수
(cffi:defcfun memset :pointer
  (ptr :pointer)
  (val :int)
  (size :int))

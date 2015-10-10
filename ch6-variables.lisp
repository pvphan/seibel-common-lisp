;; scoping with LET is 'shadowed' or 'tiered'
(defun foo (x)
  (format t "Parameter: ~a~%" x)       ; |<------- x is argument
  (let ((x (+ x 1)))                         ; |
    (format t "Outer LET: ~a~%" x)     ; | |<_---- x is 2
    (let ((x (+ x 1)))                       ; | | 
      (format t "Inner LET: ~a~%" x))  ; | | |<--- x is 3
    (format t "Outer LET: ~a~%" x))    ; | | 
  (format t "Parameter: ~a~%" x))      ; | 
(foo 1)

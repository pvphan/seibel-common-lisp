(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))
;; this is how you get the doc string:
(documentation 'verbose-sum 'function)

(verbose-sum 1 2)

;; optional parameters
(defun foo (a b &optional (c 10) (d 20)) (list a b c d))
(foo 1 2)
(foo 1 2 3)
(foo 1 2 3 4)

;; param default values based on other params
;; this is a mockup:
;; (defun make-rectangle (width &optional (height width)) ...)

;; params for testing if another param was 'supplied'
(defun foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))
(foo 1 2)
(foo 1 2 3)
(foo 1 2 4)

;; the catchall parameter symbol &rest
;;   any arguments remaining after values have been doled out to
;;   all the required and optional parameters are gathered
;;   to a list that becomes the velue of the &rest parameter
;;   (defun format (stream string &rest values) ...)
;;   (defun + (&rest numbers) ...)

;;; Keyword Parameters
(defun foo (&key a b c) (list a b c))
(foo)
(foo :a 1)
(foo :b 1)
(foo :c 1)
(foo :a 1 :c 3)
(foo :a 1 :b 2 :c 3)
(foo :a 1 :c 3 :b 2)

;; if you want named args to be different than variable names then:
;; use this pairing (&key ((:arg1 a)) ((:arg2 b) 0))
;; this is useful for decoupling the public API from the internal
;; details. this isn't very frequently used, however.
(defun foo (&key ((:apple a)) ((:box b) 0) ((:charlie c ) 0 c-supplied-p))
  (list a b c c-supplied-p))
(foo :apple 10 :box 20 :charlie 30)

;; 'flavors' of parameters must be declared in the following order:
;;   1) required
;;   2) optional
;;   3) rest
;;   4) key
;; typically, you'll combine required parameters with either &optional 
;; or &rest
;; using &optional and &key params together should be avoided (confusing)
;; a COUNTER EXAMPLE:
(defun bad-foo (x &optional y &key z) (list x y z))
(bad-foo 1 2 :z 3) ; works fine
(bad-foo 1) ; also fine
(bad-foo 1 :z 3) ; ERROR

;; rule of thumb:
;;   when you've got &optional and &key parameters, you should probably
;;   change it to use all &key params
;; warning: 4 standard functions do this! they are:
;;   1) READ-FROM-STRING
;;   2) PARSE-NAME-STRING
;;   3) WRITE-LINE
;;   4) WRITE-STRING

;; combining &rest and &key parameters
(defun foo (&rest rest &key a b c) (list rest a b c))
(foo :a 1 :b 2 :c 3)

;; return a value from anywhere in function using
;; RETURN-FROM
;; -> (foo (args) ... (return-from foo (stuff)))
(defun foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from foo (list i j))))))

(foo 12)
;; this is not commonly done, usually you let the function
;; evaluate to a value

;;; Treating functions as data
(defun foo (x) (* 2 x))
(function foo)
;; #' is syntactic sugar for FUNCTION

;; introducing FUNCALL and APPLY
;;   FUNCALL - use it when you know the number of args you're going to
;;     pass to the function
;;       i.e. (foo 1 2 3) === (funcall #'foo 1 2 3)
;;     a better example
(defun plot (fn min max step)
  (loop for i from min to max by step do
        (loop repeat (funcall fn i) do (format t "*"))
        (format t "~%")))
(plot #'exp 0 4 1/2)

;; suppos you have a list contains values you want to pass as argumenst
;; to plot. suppose this list is in the variable plot-data
;; doing it the naive way:
(plot (first plot-data) (second plot-data) (third plot-data) (fourth plot-data)) ;mockup only

;; bringing in APPLY
;; its like FUNCALL, but takes a list instead of individual args
(apply #'plot plot-data) ;mockup only

;; apply can also use "loose" arguments, so long as the last arg is a list:
(apply #'plot #'exp plot-data)

;; apply doesnt care about whether the function being applied takes 
;; &optional, &rest, or &key arguments.

;;; Anonymous Functions (LAMBDA expressions)
;; could do this
(defun double (x) (* 2 x))
(plot #'double 0 10 1)

;; but cleaner to do this
(plot #'(lambda (x) (* 2 x)) 0 10 1)

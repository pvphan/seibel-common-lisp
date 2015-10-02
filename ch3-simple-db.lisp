(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(make-cd "Roses" "Kathy Mattea" 7 t)

(defvar *db* nil)
(setf *db* nil) ;to clear memory

(defun add-record (cd) (push cd *db*))

(add-record (make-cd "Roses" "Kathy Mattea" 7 t))
(add-record (make-cd "Fly" "Dixie Chicks" 8 t))
(add-record (make-cd "Home" "Dixie Chicks" 9 t))
(add-record (make-cd "Rockin' the Suburbs" "Ben Folds" 6 t))
(add-record (make-cd "Give Us a Break" "Limpopo" 10 t))
(add-record (make-cd "Lyle Lovett" "Lyle Lovett" 9 t))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd))) 

(dump-db)

(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  (make-cd
    (prompt-read "Title")
    (prompt-read "Artist")
    (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
    (y-or-n-p "Ripped [y/n]: ")))

(defun add-cds ()
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(save-db "~/lispenv/seibel/ch3.db")

(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9 10))
;the #' means "get me the function with the following name"
; otherwise Lisp treats evenp as a variable name

; roll your own #'evenp with a lambda function
(remove-if-not (lambda (x) (= 0 (mod x 2))) '(1 2 3 4 5 6 7 8 9 10))

(remove-if-not
  #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")) *db*)

(defun select-by-artist (artist)
  (remove-if-not
    #'(lambda (cd) (equal (getf cd :artist) artist)) *db*))

(select-by-artist "Dixie Chicks")

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

;(select #'(lambda (cd) (equal (getf cd :artist) "Dixie Chicks")))
;this is gross

(defun artist-selector (artist)
  #'(lambda (cd) (equal (getf cd :artist) artist)))

(select (artist-selector "Dixie Chicks"))
; this function returns a function

; using keyword params 
(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
        (if title    (equal (getf cd :title)    title) t)
        (if artist   (equal (getf cd :artist)  artist) t)
        (if rating   (equal (getf cd :rating)  rating) t)
        (if ripped-p (equal (getf cd :ripped)  ripped) t))))

(select (where :artist "Dixie Chicks"))

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
          #'(lambda (row)
              (when (funcall selector-fn row)
                (if title    (setf (getf row :title) title))
                (if artist   (setf (getf row :artist) artist))
                (if rating   (setf (getf row :rating) rating))
                (if ripped-p (setf (getf row :ripped) ripped)))
              row) *db*)))

(update  (where :artist "Dixie Chicks") :rating 11)

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

; now introducing macros
(defmacro backwards (expr) (reverse expr))

; creates a comparison expressions
; ex:
;   in:  (make-comparison-expr :rating 10)
;   out: (EQUAL (GETF CD :RATING) 10)
(defun make-comparison-expr (field value)
  `(equal (getf cd ,field) ,value))
; think of ` as cast to high level (eval all as symbols), 
; and , as low level

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison-expr (pop fields) (pop fields))))

(defmacro where (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
; think of ,@ as 'slicing' a list
; ex. `(and ,(list 1 2 3))  ==> (AND (1 2 3))
;     `(and ,@(list 1 2 3) 4) ==> (AND 1 2 3 4))

(macroexpand-1 '(where :title "Give Us a Break" :ripped t))

; final test
(select (where :title "Give Us a Break" :ripped t))

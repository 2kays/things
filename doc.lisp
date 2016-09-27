(defpackage :#kdoc
  :use :cl)

;; "(defun add (x y)
;;    "Add two numbers."
;;    (+ x y))"

(defun docfun (expr &key (output-stream t))
  (case (car expr)
    ((defun defmacro)
     (let ((name (elt expr 1))
           (arglist (elt expr 2))
           (docstring (elt expr 3)))
       (format output-stream "## ~a~%~%### ~{~a ~}~%~@[~%~a~%~]"
               name arglist (if (stringp docstring) docstring nil))))))

;; implement file traversing through a file pointer
(defun docfuns ()
  nil)

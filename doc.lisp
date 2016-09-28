(defpackage :#kdoc
  :use :cl)

(defun docfun (expr &key (output-stream t))
  (case (car expr)
    ((defun defmacro)
     (let ((name (elt expr 1))
           (arglist (elt expr 2))
           (docstring (elt expr 3)))
       (format output-stream "## ~a~%~%### ~{~a ~}~%~@[~%~a~%~]~%"
               name arglist (if (stringp docstring) docstring nil))))))

(defun docfuns (forms)
  (dolist (form forms)
    (docfun form)))

(defun docfile (file)
  (with-open-file (s file)
    (let ((buf ""))
      (do ((l (read-line s) (read-line s nil 'eof)))
          ((eq l 'eof))
        (setf buf (concatenate 'string buf l (string #\linefeed))))
      (docfuns (read-from-string (concatenate 'string "(" buf ")"))))))


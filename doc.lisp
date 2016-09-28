(defpackage :#kdoc
  :use :cl)

;; markdown documention generator
(defun docfun (form &key output-stream)
  "Prints markdown documentation for `form` to `output-stream`."
  (case (car form)
    ((defun defmacro)
     (let ((name (elt form 1))
           (arglist (elt form 2))
           (docstring (elt form 3)))
       (format output-stream "## ~a~%~%### ~{~a ~}~%~@[~%~a~%~]~%"
               name arglist (if (stringp docstring) docstring nil))))
    (otherwise "")))

(defun docfuns (forms)
  "Document a list of forms"
  (apply #'concatenate 'string
               (mapcar #'(lambda (form) (docfun form)) forms)))

(defun docfile (file)
  (with-open-file (s file)
    (let ((buf ""))
      (do ((l (read-line s) (read-line s nil 'eof)))
          ((eq l 'eof))
        (setf buf (concatenate 'string buf l (string #\linefeed))))
      (docfuns (read-from-string (concatenate 'string "(" buf ")"))))))


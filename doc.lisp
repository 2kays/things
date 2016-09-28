(defpackage :#kdoc
  (:use :cl)
  (:export :docfun
           :docfuns
           :docfile))

(in-package :kdoc)

(declaim (inline cat))
(defun cat (&rest list)
  "Shorthand for `concatenate 'string`."
  (apply #'concatenate 'string list))

;; markdown documention generator
;; (with-open-file (s "~/test.html" :direction :output)
;;   (markdown:markdown (docfile "/home/k/code/things/doc.lisp") :stream s))

(defun docfun (form &key output-stream)
  "Prints markdown documentation for `form` to `output-stream`."
  (case (car form)
    ((defun defmacro defparameter defvar defconstant)
     (let ((name (elt form 1))
           (arglist (elt form 2))
           (docstring (unless (< (length form) 4) (elt form 3))))
       (format output-stream "## ~a~%~%### ~{*~s*~^, ~}~%~@[~%~a~%~]~%"
               name
               (if (listp arglist) arglist (list arglist))
               (if (stringp docstring) docstring nil))))
    (otherwise "")))

(defun docfuns (forms)
  "Document a list of forms `forms`."
  (apply #'cat (mapcar #'(lambda (form) (docfun form)) forms)))

(defun docfile (file)
  "Generates markdown documentation for path `file`."
  (with-open-file (s file)
    (let ((buf ""))
      (do ((l (read-line s) (read-line s nil 'eof)))
          ((eq l 'eof))
        (setf buf (cat buf l (string #\linefeed))))
      (docfuns (read-from-string (cat "(" buf ")"))))))


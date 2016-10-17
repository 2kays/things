(defpackage #:keditor
  (:use :cl :asdf :cl-charms)
  (:export main))

(in-package :keditor)

;; (ql:quickload '(:swank) :silent t)
;; (swank:create-server :port 4006 :dont-close t) 

;; Concepts:
;;  STATE - list of lines, the buffer contents
;;  CURSOR - x, y, constrained to the bounds of the state
;;           cursor is moved for standard editing commands
;;

(defun concat (&rest args)
  (apply #'concatenate 'string args))

(defun file-to-list (path)
  (with-open-file (f path)
    (loop :for l := (read-line f nil)
       :while l
       :collect l)))

(defun file-to-string (path)
  (with-open-file (f path)
    (let ((str (make-string (file-length f))))
      (read-sequence str f)
      str)))

(defun insert-at (item list index)
  (cond
    ((< index 1) (error "Index too small ~A" index))
    ((= index 1) (cons item list))
    (t (push item (cdr (nthcdr (- index 2) list)))
       list)))

(defparameter *current-buffer* nil)
(defparameter *file* nil)

(defun write-mode-line ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (charms:write-string-at-point charms:*standard-window*
                                  "Kek"
                                  0 (1- h))))

(defun state-to-string (state)
  (reduce (lambda (s1 s2) (format nil "~a~%~a" s1 s2)) state))

(defun clamp (x y state)
  (if (< x 0) (setf x 0))
  (if (< y 0) (setf y 0))
  (if (> y (1- (length state))) (setf y (1- (length state))))
  (if (> x (length (elt state y))) (setf x (length (elt state y))))
  (values x y))

(defun main (&rest argv)
  (if (not argv)
      (format t "error: no file specified~%")
      (let* ((filepath (first argv))
             (file-state (file-to-list filepath)))
        (charms:with-curses ()
          (charms:clear-window charms:*standard-window* :force-repaint t)
          (charms:disable-echoing)
          (charms:enable-raw-input :interpret-control-characters t)
          (charms:enable-non-blocking-mode charms:*standard-window*)
          
          (loop :named driver-loop
             :with x := 0
             :with y := 0
             :for c := (charms:get-char charms:*standard-window* :ignore-error t)
             :do (progn
                   (charms:refresh-window charms:*standard-window*)
                   ;;(write-mode-line)
                   (case c
                     ((nil) nil)
                     ;; C-fbpn movement
                     ((#\So) (incf y))
                     ((#\Dle) (decf y))
                     ((#\Ack) (incf x))
                     ((#\Stx) (decf x))
                     ;; Return
                     ((#\Lf) (let* ((line (elt file-state y))
                                    (l1 (subseq file-state 0 y))
                                    (l2 (subseq file-state (1+ y)))
                                    (s1 (subseq line 0 x))
                                    (s2 (subseq line x)))
                               (setf file-state (concatenate 'list l1 (list s1) (list s2) l2))
                               (incf y)
                               (setf x 0)))
                     ;; Backspace
                     ((#\Del) (let* ((line (elt file-state y))
                                     (s1 (subseq line 0 (1- x)))
                                     (s2 (subseq line x)))
                                (setf (elt file-state y) (format nil "~a~a" s1 s2))
                                (decf x)))
                     ;; C-d / delete
                     ;; TODO: refactor this string splicing into one function
                     ((#\Eot #\Bs) (let* ((line (elt file-state y))
                                          (s1 (subseq line 0 x))
                                          (s2 (subseq line (1+ x))))
                                     (setf (elt file-state y) (format nil "~a~a" s1 s2))))
                     ;; C-x quits 
                     ((#\Can) (return-from driver-loop))
                     ;; 32 to 126 are printable characters
                     (t (if (and (> (char-code c) 31) (< (char-code c) 127))
                            (let* ((line (elt file-state y))
                                   (s1 (subseq line 0 x))
                                   (s2 (subseq line x)))
                              (setf (elt file-state y) (format nil "~a~a~a" s1 c s2))
                              (incf x)))))
                   (multiple-value-bind (cx cy) (clamp x y file-state)
                     (setf x cx)
                     (setf y cy)
                     (charms:write-string-at-point charms:*standard-window*
                                                   (state-to-string file-state) 0 0)
                     (charms:move-cursor charms:*standard-window* cx cy))))))))

;;
;; Yet Another Messy Amateur Text Editor
;; YAMATE
;;
;; BabEmacs/Babbymacs
;;
;; Feed Me Parens
;; FMP
;;


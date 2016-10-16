(defpackage #:keditor
  (:use :cl :asdf :cl-charms)
  (:export main))

(in-package :keditor)

#|
(ql:quickload '(:swank) :silent t)
(swank:create-server :port 4006 :dont-close t)
|# 

(defun concat (&rest args)
  (apply #'concatenate 'string args))

(defun file-to-string (path)
  (with-open-file (f path)
    (let ((str (make-string (file-length f))))
      (read-sequence str f)
      str)))

(defparameter *current-buffer* nil)
(defparameter *file* nil)

(defun write-mode-line ()
  (multiple-value-bind (w h)
      (charms:window-dimensions charms:*standard-window*)
    (charms:write-string-at-point charms:*standard-window*
                                  "Kek"
                                  0 (1- h))))

(defun main (&rest argv)
  (if (not argv)
      (format t "error: no file specified~%")
      (let* ((filepath (first argv))
             (file-contents (file-to-string filepath)))
        (charms:with-curses ()
          (charms:clear-window charms:*standard-window* :force-repaint t)
          (charms:disable-echoing)
          (charms:enable-raw-input :interpret-control-characters t)
          (charms:enable-non-blocking-mode charms:*standard-window*)
          (charms:write-string-at-point charms:*standard-window* file-contents 0 0)
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
                     ;; C-d
                     ((#\Eot) (decf x) (charms:write-char-at-point charms:*standard-window* #\Sp x y))
                     ;; C-x quits 
                     ((#\Can) (return-from driver-loop))
                     (t (if (and (> (char-code c) 31) (< (char-code c) 127))
                            (progn
                              (insert-char-at-cursor charms:*standard-window* c)
                              (incf x)))))
                   
                   (charms:move-cursor charms:*standard-window* x y)))))))

;; Yet Another Messy Amateur Text Editor
;; YAMATE
;;
;; Fully Extensible Text Insertion System H..
;; FETISH
;;
;; It's Like Emacs But Sh*t
;; ILEBS
;;
;; BabEmacs/Babbymacs
;;
;; Feed Me Parens
;; FMP
;;

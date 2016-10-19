(defpackage #:keditor
  (:use :cl :asdf :cl-charms)
  (:export main))

(in-package :keditor)

;; (ql:quickload '(:swank) :silent t)
;; (swank:create-server :port 4006 :dont-close t)
;;
;; Concepts:
;;  STATE - list of lines, the buffer contents
;;  CURSOR - x, y, constrained to the bounds of the state
;;           cursor is moved for standard editing commands
;;

(defstruct (buffer (:conc-name buf-))
  (name "buffer" :type string)
  (modified nil :type boolean)
  (state '() :type list)
  (cursor-x 0 :type integer)
  (cursor-y 0 :type integer))

(defparameter *current-buffer* nil)
(defun current-buffer () *current-buffer*)

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

(defun state-to-string (state)
  "Reduce editor state by flattening STATE to a string, inserting newlines."
  (reduce (lambda (s1 s2) (format nil "~a~%~a" s1 s2)) state))

(defun clamp (x y state)
  "Clamp the cursor with position X Y to the bounds of the editor state STATE."
  (if (< x 0) (setf x 0))
  (if (< y 0) (setf y 0))
  (if (> y (1- (length state))) (setf y (1- (length state))))
  (if (> x (length (elt state y))) (setf x (length (elt state y))))
  (values x y))

;; split a sequence SEQ at POS
(defun split-at (seq pos)
  "Split a sequence SEQ at position POS."
  (let ((s1 (subseq seq 0 pos))
        (s2 (subseq seq pos)))
    (list s1 s2)))

(defun split-many (seq &rest positions)
  "Split a sequence SEQ at POSITIONS."
  (labels ((sm-helper (seq offset positions)
             (if (null positions)
                 (list seq)
                 (let ((pos (first positions)))
                   (destructuring-bind (s1 s2)
                       (split-at seq (- pos offset))
                     (cons s1 (sm-helper s2 pos (rest positions))))))))
    (sm-helper seq 0 positions)))

(defparameter *key-map*
  '((#\So . (lambda (&rest a)))))

(defun main (&optional argv)
  "Entrypoint for the editor. ARGV should contain a file path."
  (let ((bname (or argv "buffer1"))
        (bstate (if argv
                    (file-to-list argv)
                    (list ""))))
    (setf *current-buffer*
          (make-buffer :name bname :state bstate)))
  (charms:with-curses ()
    (charms:clear-window charms:*standard-window* :force-repaint t)
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (loop :named driver-loop
       :for c := (charms:get-char charms:*standard-window* :ignore-error t)
       :do
       (charms:refresh-window charms:*standard-window*)
       (with-accessors ((name buf-name) (x buf-cursor-x)
                        (y buf-cursor-y) (state buf-state))
           *current-buffer*
         (case c
           ((nil) nil)
           ;; C-fbpn movement
           ((#\So) (incf y))
           ((#\Dle) (decf y))
           ((#\Ack) (incf x))
           ((#\Stx) (decf x))
           ;; Meta
           ((#\Esc) (setf (elt state y) "Meta"))
           ;; Return
           ((#\Lf) (let* ((line (elt state y))
                          (l1 (subseq state 0 y))
                          (l2 (subseq state (1+ y)))
                          (s1 (subseq line 0 x))
                          (s2 (subseq line x)))
                     (setf state (concatenate 'list l1 (list s1) (list s2) l2))
                     (incf y)
                     (setf x 0)))
           ;; Backspace
           ((#\Del) (destructuring-bind (s1 _ s2)
                        (split-many (elt state y) (1- x) x)
                      (setf (elt state y) (concat s1 s2))
                      (decf x)))
           ;; C-d / delete
           ;; TODO: refactor this string splicing into one function
           ((#\Eot #\Bs) (destructuring-bind (s1 _ s2)
                             (split-many (elt state y) x (1+ x))
                           (setf (elt state y) (concat s1 s2))))
           ;; C-x quits
           ((#\Can) (return-from driver-loop))
           ;; 32 to 126 are printable characters
           (t (if (and (> (char-code c) 31) (< (char-code c) 127))
                  (destructuring-bind (s1 s2)
                      (split-at (elt state y) x)
                    (setf (elt state y) (format nil "~a~a~a" s1 c s2))
                    (incf x)))))
         (multiple-value-bind (cx cy) (clamp x y state)
           (setf x cx)
           (setf y cy)
           (charms:write-string-at-point charms:*standard-window*
                                         (state-to-string state) 0 0)
           (charms:move-cursor charms:*standard-window* x y))))))
;;
;; Yet Another Messy Amateur Text Editor
;; YAMATE
;;
;; BabEmacs/Babbymacs
;;
;; Feed Me Parens
;; FMP
;;


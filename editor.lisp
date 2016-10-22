(defpackage #:babbymacs                 ; Yeah...
  (:use :cl :asdf :cl-charms)
  (:export main))

(in-package :babbymacs)

"
Easy REPL setup - why doesn't paredit like #| |# ?
(ql:quickload '(:swank :cl-charms) :silent t)
(swank:create-server :port 4006 :dont-close t)
"

;; Concepts:
;;  * STATE - array of strings (lines), the buffer contents
;;  * CURSOR - x, y, constrained to the bounds of the state
;;           cursor is moved for standard editing commands
;; 
;; TODO:
;;  * Multiple ncurses windows: buffer (with scrolling), modeline
;;  * Handle meta key properly, with more complex keymaps
;;  * Potentially implement major/minors? I want to have modes for editing, but
;;    also for general stuff like no-input, unbounded cursor movement, etc.
;;  * Primitive CL mode with a SWANK client!
;;  * Colours!

(defstruct (buffer (:conc-name buf-))
  (name "buffer" :type string)
  (modified nil :type boolean)
  (state (make-array 1 :element-type 'string :initial-element ""))
  (cursor-x 0 :type integer)
  (cursor-y 0 :type integer)
  (furthest-x 0 :type integer))

(defparameter *editor-running* nil
  "The editor's running state.")

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

(defun state-to-string (state &key newline)
  "Reduce editor state by flattening STATE to a string with newlines, with the 
key argument NEWLINE specifying if an additional newline is added to the end."
  (concat (format nil "~{~a~^~%~}" state) (and newline (string #\nl))))

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

(defun remove-at (seq pos &key (count 1))
  "Removes COUNT entries at position POS of SEQ."
  (remove-if (constantly t) seq :start pos :count count))

;;; Beginning of editor commands

(defun forward (&optional (delta 1))
  "Moves the cursor forward."
  (labels ((forward-1 (del)
             (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                              (state buf-state) (fx buf-furthest-x))
                 (current-buffer)
               (incf x del)
               (cond ((and (> x (length (elt state y)))
                           (< y (1- (length state))))
                      ;; wrap to next line if we aren't on the last one
                      (incf y)
                      (setf x 0))
                     ((and (< x 0) (> y 0))
                      ;; wrap to the previous line if we're not on the first
                      (decf y)
                      (setf x (length (elt state y)))))
               (setf fx x))))
    ;; get the sign of delta, loop for |delta| and multiply by sign
    ;; allows us to move backward without separate handling of neg delta
    (let ((sign (signum delta)))
      (dotimes (v (abs delta))
        (forward-1 (* 1 sign))))))

(defun backward (&optional (delta 1))
  "Moves the cursor backward."
  (forward (* delta -1)))

;; TODO: bounds checks on y = 0 / y = length state
;; TODO: handle delta correctly
(defun down (&optional (delta 1))
  "Moves the cursor down."
  (labels ((down-1 (del)
             (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                              (state buf-state) (fx buf-furthest-x))
                 (current-buffer)
               ;; bounds checking: first clause is up, second is down
               (when (or (and (< del 0) (> y 0))
                         (and (> del 0) (< y (1- (length state)))))
                 (incf y del)
                 ;; handle furthest column
                 (setf x (min fx (length (elt state y))))))))
    (let ((sign (signum delta)))
      (dotimes (v (abs delta))
        (down-1 (* 1 sign))))))

(defun up (&optional (delta 1))
  "Moves the cursor up."
  (down (* delta -1)))

(defun backspace ()
  "Backspaces from cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (if (zerop x)
        (let ((line (elt state y))
              (prevline (elt state (1- y))))
          (setf (elt state (1- y)) (concat prevline line))
          (setf state (remove-at state y))
          (decf y)
          (setf x (1+ (length prevline))))
        (setf (elt state y) (remove-at (elt state y) (1- x))))
    (decf x)))

(defun delete-char ()
  "Deletes char at cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (setf (elt state y) (remove-at (elt state y) x))))

(defun newline ()
  "Inserts a newline at cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer) 
    (let* ((line (elt state y))
           (l1 (subseq state 0 y))
           (l2 (subseq state (1+ y)))
           (s1 (subseq line 0 x))
           (s2 (subseq line x)))
      (setf state (concatenate 'list l1 (list s1) (list s2) l2))
      (incf y)
      (setf x 0))))

(defun exit-editor (&optional force)
  "Exits the editor."
  (if (not force) nil) ;; change this to ask the user if they're sure
  (setf *editor-running* nil))

(defun line-end ()
  "Jumps to the end of a line."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (setf x (length (elt state y)))))

(defun line-beginning ()
  "Jumps to the beginning of a line."
  (setf (buf-cursor-x (current-buffer)) 0))

;;; End of editor commands

(defparameter *key-map*
  '((#\Ack . forward)                   ; C-f
    (#\Stx . backward)                  ; C-b
    (#\So . down)                       ; C-n
    (#\Dle . up)                        ; C-p
    (#\Del . backspace)                 ; backspace
    (#\Eot . delete-char)               ; C-d
    (#\Bs . delete-char)                ; delete
    (#\Can . exit-editor)               ; C-x
    (#\Lf . newline)                    ; return
    (#\Enq . line-end)                  ; C-e
    (#\Soh . line-beginning)            ; C-a
    ))

(defun main (&optional argv)
  "Entrypoint for the editor. ARGV should contain a file path."
  (setf *editor-running* t)
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
       :while *editor-running*
       :for c := (charms:get-char charms:*standard-window* :ignore-error t)
       :do
       (charms:refresh-window charms:*standard-window*)
       (with-accessors ((name buf-name) (x buf-cursor-x)
                        (y buf-cursor-y) (state buf-state))
           (current-buffer)
         (let ((entry (assoc c *key-map*)))
           (when entry (funcall (cdr entry))))
         (case c
           ((nil) nil)
           ;; Meta
           ((#\Esc) (setf (elt state y) "Meta"))
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
                                         (state-to-string state :newline t) 0 0) 
           (charms:move-cursor charms:*standard-window* x y))))))


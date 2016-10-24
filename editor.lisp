(defpackage #:babbymacs
  (:use :cl :asdf :cl-charms)
  (:export main))

(in-package :babbymacs)

"
Easy REPL setup - why dpoesn't paredit like #| |# ?
(ql:quickload '(:swank :cl-charms) :silent t)
(swank:create-server :port 4006 :dont-close t)
"

;; Concepts:
;;  * STATE - array of strings (lines), the buffer contents
;;  * CURSOR - x, y, constrained to the bounds of the state
;;           cursor is moved for standard editing commands
;; 
;; TODO:
;;  * Unit tests!!!
;;  * Multiple ncurses windows: buffer (with scrolling), modeline
;;  * Handle meta key properly, with more complex keymaps
;;     - Currently we can handle C-, M- and C-M- prefixes.
;;  * Potentially implement major/minors? I want to have modes for editing, but
;;    also for general stuff like no-input, unbounded cursor movement, etc.
;;  * Primitive CL mode with a SWANK client!
;;  * Colours!
;;  * Convert to CLOS? Modes might be a lot easier
;;

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
               ;; don't go further back if we're at the start
               ;; ...or further forward if we're at the end
               (unless (or (and (= x 0) (= y 0) (> 0 del))
                           (and (= x (length (elt state y)))
                                (= y (1- (length state)))
                                (< 0 del)))
                (incf x del))
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
    (backward)))

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
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y)
                   (state buf-state) (fx buf-furthest-x))
      (current-buffer)
    (setf x (length (elt state y))
          fx x)))

(defun line-beginning ()
  "Jumps to the beginning of a line."
  (setf (buf-cursor-x (current-buffer)) 0
        (buf-furthest-x (current-buffer)) 0))

(defun insert-char (c)
  "Inserts a character at the cursor position."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (destructuring-bind (s1 s2)
        (split-at (elt state y) x)
      (setf (elt state y) (format nil "~a~a~a" s1 c s2))
      (forward))))

(defun run-command ()
  "Run a command."
  (forward 2))

;;; Meta handling

(defparameter *meta-pressed* nil
  "Describes if the meta key has been pressed prior.")

(defun meta ()
  "Handles presses of the meta key."
  (setf *meta-pressed* t))

(defun scroll-up ()
  (charms/ll:wscrl (charms::window-pointer charms:*standard-window*) -1))
;;; End of editor commands

(defparameter *meta-map*
  '((#\x . run-command)
    (#\p . scroll-up)))

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
    (#\Esc . meta)                      ; meta key (alt/esc)
    ))

(defun main2 (&optional argv)
  (charms:with-curses ()
    (charms:clear-window charms:*standard-window* :force-repaint t)
    (charms:disable-echoing)
    (charms:enable-raw-input :interpret-control-characters t)
    (charms:enable-non-blocking-mode charms:*standard-window*)
    (let* ((pad (charms/ll:newpad 300 150)))
      (charms/ll:mvwaddstr pad 0 0 (file-to-string argv))
      (loop :named driver
         :with x := 0
         :with y := 0
         :for c := (charms:get-char charms:*standard-window* :ignore-error t)
;         :for wptr := (charms/ll:subpad pad 20 20 x y)
         :do
         (charms/ll:prefresh pad y x 0 0 50 50)
         (charms:refresh-window charms:*standard-window*)
         (case c
           ((nil) nil)
           ((#\q) (return-from driver))
           ((#\n) (incf y))
           ((#\p) (decf y)))
         
         (when nil (charms/ll:wborder pad
                             (char-int #\|) (char-int #\|)
                             (char-int #\-) (char-int #\-)
                             (char-int #\+) (char-int #\+)
                             (char-int #\+) (char-int #\+)))
         ))))

(defun main (&optional argv)
  "Entrypoint for the editor. ARGV should contain a file path."
  ;; override global state that may already be setn
  (setf *editor-running* t)
  (setf *meta-pressed* nil)
  ;; if argv is set, open that file, else create an empty buffer
  (let ((bname (or argv "buffer1"))
        (bstate (if argv
                    (file-to-list argv)
                    (list ""))))
    (setf *current-buffer*
          (make-buffer :name bname :state bstate)))
  (charms:with-curses ()
    (charms/ll:scrollok (charms::window-pointer charms:*standard-window*) 1)
    ;;(charms/ll:idlok (charms::window-pointer charms:*standard-window*) 1)
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
         ;; if we previously pressed the meta key, resolve commands from the
         ;; meta map, otherwise use the standard root key map
         (let* ((map (if *meta-pressed* *meta-map* *key-map*))
                (entry (assoc c map)))
           (cond ((null c) nil)         ; ignore nils
                 ;; 32->126 are printable, so print c if it's not a part of a
                 ;; meta command
                 ((and (> (char-code c) 31)
                       (< (char-code c) 127)
                       (not *meta-pressed*))
                  (insert-char c))
                 ;; if the entry for the keymap has resolved to something, run it
                 (entry
                  (setf *meta-pressed* nil)
                  (funcall (cdr entry)))))
         ;; (charms:write-string-at-point charms:*standard-window* (state-to-string state :newline t) 0 0)
         (charms/ll:mvwaddstr (charms::window-pointer charms:*standard-window*)
                              x y (state-to-string state :newline t))
         
         
         (charms:move-cursor charms:*standard-window* x y)))))


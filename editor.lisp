(defpackage #:babbymacs
  (:use :cl :asdf :cl-charms :cffi)
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
;;  * Multiple ncurses windows: buffer (with scrolling), modeline (DONE-ISH)
;;     - Modeline should be have color option
;;     - Command to shrink/enlarge modeline, with separate parts
;;     - External formatting of the modeline (DONE-ISH)
;;  * Handle meta key properly, with more complex keymaps (DONE-MOSTLY)
;;  * Potentially implement major/minors? I want to have modes for editing, but
;;    also for general stuff like no-input, unbounded cursor movement, etc.
;;  * Primitive CL mode with a SWANK client!
;;  * Colours!
;;  * Convert to CLOS? Modes might be a lot easier
;;  * Refactor movement and insertion commands (IN-PROGRESS)
;;

(defclass buffer* ()
  ((name :accessor buf-name
         :initform "buffer"
         :initarg :name
         :type string)
   (state :accessor buf-state
          :initform (make-array 1 :element-type 'string :initial-element ""
                                :adjustable t :fill-pointer t)
          :initarg :state)
   (cursor-x :accessor buf-cursor-x :initform 0 :type integer)
   (cursor-y :accessor buf-cursor-y :initform 0 :type integer)
   (furthest-x :accessor buf-furthest-x :initform 0 :type integer)))

(defconstant +SIGINT+ 2
  "SIGINT UNIX signal code.")
 
(defmacro set-signal-handler (signo &body body)
  "Generates a signal handler for SIGNO from BODY."
  ;; Lifted from stackoverflow
  (let ((handler (gensym "HANDLER")))
    `(progn
       (cffi:defcallback ,handler :void ((signo :int))
         (declare (ignore signo))
         ,@body)
       (cffi:foreign-funcall "signal" :int ,signo :pointer (cffi:callback ,handler)))))

(defparameter *current-buffer* nil)
(defun current-buffer ()
  "Returns the current buffer."
  *current-buffer*)

(defparameter *editor-running* nil
  "The editor's running state.")

(defparameter *modeline-height* 1
  "The height of the mode line.")

(defparameter *modeline-format*
  '("%p% (%x,%y) : %b")
  "Describes the format of the modeline at various sizes.")

(defun modeline-formatter ()
  "Returns the formatted modeline strings."
  (with-accessors ((name buf-name) (x buf-cursor-x) (y buf-cursor-y)
                   (state buf-state))
      (current-buffer)
    ;; TODO: this is awful, refactor
    (loop :with spec := (list (cons "%p" (write-to-string
                                          (truncate (* (/ y (max 1 (1- (length state))))
                                                       100))))
                              (cons "%x" (write-to-string x))
                              (cons "%y" (write-to-string y))
                              (cons "%b" name))
       :for s :in *modeline-format*
       :collect (loop :with modified := s
                   :for (k . v) :in spec
                   :do (setf modified (replace-all modified k v))
                   :finally (return modified)))))

(defparameter *current-keymap* nil
  "The current keymap for input lookups.")

(defun concat (&rest args)
  "Concatenates ARGS to a string."
  (apply #'concatenate 'string args))

(defun stream-to-list (stream)
  "Returns a string stream STREAM to a list, handling empty lines followed by
EOF properly."
  (loop :for (l s) := (multiple-value-list (read-line stream nil))
     :collect (if s (or l "") l)
     :until s))

(defun file-to-list (path)
  "Returns a list of lines of the file at PATH."
  (with-open-file (f path)
    (stream-to-list f)))

(defun file-to-string (path)
  "Dumps file at PATH to a string and returns it."
  (with-open-file (f path)
    (let ((str (make-string (file-length f))))
      (read-sequence str f)
      str)))

(defun state-to-string (state &key newline)
  "Reduce editor state by flattening STATE to a string with newlines, with the
key argument NEWLINE specifying if an additional newline is added to the end."
  (concat (format nil "~{~a~^~%~}" (coerce state 'list)) (and newline (string #\nl))))

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

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop :with part-length := (length part)
          :for old-pos = 0 :then (+ pos part-length)
          :for pos := (search part string
                            :start2 old-pos
                            :test test)
          :do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          :when pos :do (write-string replacement out)
       :while pos)))

(defun remove-at (seq pos &key (count 1))
  "Removes COUNT entries at position POS of SEQ."
  (remove-if (constantly t) seq :start pos :count count))

(defun string-insert-at* (string pos elem)
  "Inserts ELEM into STRING at POS."
  (format nil "~a~a~a" (subseq string 0 pos) elem (subseq string pos)))

(defun insert-into-array (vector value position)
  "Inserts VALUE into VECTOR at POSITION."
  (replace vector vector :start2 position :start1 (1+ position) 
           :end2 (vector-push-extend value vector))
  (setf (aref vector position) value) 
  vector)

(defun remove-from-array (vector position)
  "Removes element at POSITION from VECTOR."
  (replace vector vector :start2 (1+ position) :start1 position)
  (vector-pop vector)
  vector)

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

(defun join-lines (&optional (offset 1))
  "Join the current line with the line at OFFSET."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (when (<= 0 (+ y offset) (1- (length state)))
     (let ((line (elt state y))
           (jline (elt state (+ y offset))))
       (setf (elt state (+ y offset)) (concat jline line))
       (remove-from-array state y)))))

(defun backspace ()
  "Backspaces from cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (cond ((zerop x)
           (join-lines -1)
           (decf y)
           (setf x (1+ (length (elt state y)))))
          (t (setf (elt state y) (remove-at (elt state y) (1- x)))))
    (backward)))

(defun delete-char ()
  "Deletes char at cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (cond ((= x (length (elt state y)))
           (join-lines 1)
           (setf x 0))
          (t (setf (elt state y) (remove-at (elt state y) x))))))

(defun newline ()
  "Inserts a newline at cursor."
  (with-accessors ((x buf-cursor-x) (y buf-cursor-y) (state buf-state))
      (current-buffer)
    (destructuring-bind (s1 s2) (split-at (elt state y) x)
      (setf (elt state y) s1)
      (insert-into-array state s2 (1+ y))
      (down)
      (line-beginning))))

(defun exit-editor (&optional force)
  "Exits the editor."
  (if (not force) nil) ;; change this to ask the user if they're sure
  (format t "Exiting..~%")
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

(defparameter *command-typed* nil)
(defun run-command ()
  "Run a command input by the user. Hijacks the current key input."
  (setf *command-typed* (make-array 0 :fill-pointer t :adjustable t
                                    :element-type 'character))
  (let ((theight 1) (twidth 1))
    (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
    (let ((cmdwin (charms/ll:newwin 1 64 (1- theight) 0)))
      (charms/ll:wattron cmdwin (charms/ll:color-pair 1))
      (loop :named cmd-loop
         :while *editor-running*
         :for c := (charms:get-char charms:*standard-window* :ignore-error t)
         :do
         (charms/ll:werase cmdwin)
         (charms/ll:waddstr cmdwin (concat "Command: " *command-typed*))
         (cond ((null c) nil)
               ((and (> (char-code c) 31)
                     (< (char-code c) 127))
                (vector-push-extend c *command-typed*))
               ((eql c #\Bel) (setf *command-typed* nil) (return-from cmd-loop))
               ((eql c #\Del) (vector-pop *command-typed*))
               (t (return-from cmd-loop)))
         (charms/ll:wrefresh cmdwin))
      (charms/ll:wattron cmdwin (charms/ll:color-pair 1))
      (charms/ll:delwin cmdwin)
      (and *command-typed* (eval (read-from-string *command-typed*))))))

;;; End of editor commands

(defparameter *meta-map*
  `((#\x . ,#'run-command)
    ))

(defparameter *c-x-map*
  `((#\Etx . ,#'exit-editor)            ; C-x C-c (NOT WORKING)
    (#\q   . ,#'exit-editor)            ; C-x q
    ))

(defparameter *root-keymap*
  `((#\Ack . forward)                   ; C-f
    (#\Stx . backward)                  ; C-b
    (#\So  . down)                      ; C-n
    (#\Dle . up)                        ; C-p
    (#\Del . backspace)                 ; backspace
    (#\Eot . delete-char)               ; C-d
    (#\Bs  . delete-char)               ; delete
    (#\Can . ,*c-x-map*)                ; C-x
    (#\Lf  . newline)                   ; return
    (#\Enq . line-end)                  ; C-e
    (#\Soh . line-beginning)            ; C-a
    (#\Esc . ,*meta-map*)               ; meta key (alt/esc)
    ))

(defun resolve-key (c)
  "Resolves an input key C to a command or nested keymap according to the
current global keymap."
  (let* ((entry-pair (assoc c (if *current-keymap*
                                  *current-keymap*
                                  *root-keymap*))))
    ;; if the entry for the keymap has resolved to something
    ;; if it's a function/symbol, run it
    ;; if it's a list, set the current keymap to it
    (if entry-pair
        (let ((entry (cdr entry-pair)))
          (cond ((or (functionp entry)
                     (symbolp entry))
                 (funcall entry)
                 (setf *current-keymap* nil))
                ((consp entry)
                 (setf *current-keymap* entry))
                (t (setf *current-keymap* nil))))
        (setf *current-keymap* nil))))

(defun main (&optional argv)
  "Entrypoint for the editor. ARGV should contain a file path."
  ;; Resolve C-c SIGINTs to C-c in the keymap
  (set-signal-handler +SIGINT+ (resolve-key #\Etx))
  
  (setf *editor-running* t)
  (setf *modeline-height* 1)
  (setf *current-keymap* nil)
  ;; if argv is set, open that file, else create an empty buffer
  (let ((bname (or argv "buffer1"))
        (bstate (if argv
                    (file-to-list argv)
                    (list ""))))
    (setf *current-buffer*
          (make-instance 'buffer*
                         :name bname
                         :state (make-array (length bstate) :element-type 'string
                                            :fill-pointer t
                                            :adjustable t
                                            :initial-contents bstate))))
  (charms:with-curses ()
    (charms/ll:start-color)
    (let ((theight 1)
          (twidth 1))
      ;; Set initial terminal size
      (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
      (charms/ll:use-default-colors)
      (charms/ll:init-pair 1 charms/ll:color_white charms/ll:color_black)
      ;; Build the pad according to the file state
      ;; TODO: programmatically determine column max (150 is reasonable for now)
      ;; EXPLANATION FOR FUTURE ME
      ;; Say we have theight of 50, file is 70 lines. We want to allocate the
      ;; pad to be a multiple of theight that is enough to accomodate the lines.
      ;; So we take `ceil(theight / lines)`, which gives us that multiple.
      ;; We multiply theight by that for the pad height.
      ;; TODO: dynamically react to terminal height changes when allocating pad
      (let* ((page-cnt (ceiling (length (buf-state (current-buffer))) theight))
             (pad (charms/ll:newpad (* theight page-cnt) 150))
             (mlwin (charms/ll:newwin *modeline-height* (1- twidth) (- theight *modeline-height*) 0)))
        ;; Set up terminal behaviour
        ;;        (charms:clear-window charms:*standard-window* :force-repaint t)
        (charms:disable-echoing)
        (charms:enable-raw-input :interpret-control-characters t)
        (charms:enable-non-blocking-mode charms:*standard-window*)
        (loop :named driver-loop
           :while *editor-running*
           :for c := (charms:get-char charms:*standard-window* :ignore-error t)
           :do
           ;; Update terminal height and width
           (charms/ll:getmaxyx charms/ll:*stdscr* theight twidth)
           (with-accessors ((name buf-name) (x buf-cursor-x)
                            (y buf-cursor-y) (state buf-state))
               (current-buffer)
             ;; if we previously pressed the meta key, resolve commands from the
             ;; meta map, otherwise use the standard root key map
             ;;(if c (format t "received ~s~%" c))
             (cond ((null c) nil)       ; ignore nils
                   ;; 32->126 are printable, so print c if it's not a part of
                   ;; a meta command
                   ((and (> (char-code c) 31)
                         (< (char-code c) 127)
                         (not *current-keymap*))
                    (insert-char c))
                   (t (resolve-key c)))
             ;; write the updated file state to the pad and display it at the
             ;; relevant y level
             (let* ((mlh *modeline-height*)
                    (winh (- theight mlh 1))
                    (mstrs (modeline-formatter)))
               ;; Draw the modeline
               (unless (zerop mlh)
                 ;; oh no please don't tell me I need multiple ncurses windows
                 ;; to implement multiple modelines...
                 (charms/ll:werase mlwin)
                 (charms/ll:wattron mlwin (charms/ll:color-pair 1))
                 (dotimes (mline mlh)
                   (let ((mstr (elt mstrs mline)))                     
                     (charms/ll:mvwaddstr mlwin 0 (- twidth (length mstr) 1)
                                          mstr)))
                 (charms/ll:wattroff mlwin (charms/ll:color-pair 1)))
               ;; (charms/ll:wbkgd mlwin (charms/ll:color-pair 1))
               (charms/ll:werase pad)
               (charms/ll:mvwaddstr pad 0 0 (state-to-string state))
               (charms/ll:wmove pad y x)
               (charms/ll:wnoutrefresh mlwin)
               (charms/ll:pnoutrefresh pad (* winh (floor (/ y winh))) 0 0 0
                                       (1- winh) (- twidth 1))
               (charms/ll:doupdate))))
        ;; Cleanup
        ;; (charms/ll:init-pair 1 charms/ll:color_black charms/ll:color_white)
        (charms/ll:delwin pad)
        (charms/ll:delwin mlwin)
        (charms/ll:standend)))))

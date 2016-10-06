(defpackage #:kserv
  (:use :cl
        :cl-async
        :usocket))

(in-package :kserv)

(defparameter *tcp-server-instance* nil)

(defun my-echo-server ()
  (format t "Starting server.~%")
  (setf *tcp-server-instance*
        (as:tcp-server nil 9003         ;; nil is "0.0.0.0"
                       (lambda (socket data)
                         ;; echo the data back into the socket
                         (format t "Received ~a~%" data)
                         (as:write-socket-data socket "Shaddap!"
                                               :write-cb
                                               (lambda (sock)
                                                 (as:close-socket sock))))
                       :event-cb
                       (lambda (err)
                         (format t "listener event: ~a~%" err))))
  (as:signal-handler 2 (lambda (sig)
                         (declare (ignore sig))
                         (as:exit-event-loop))))

(as:start-event-loop #'my-echo-server)

(defun client ()
  (let ((stream (usocket:socket-connect "127.0.0.1" 9003)))
    (princ "Hello!" (usocket:socket-stream stream))
    (force-output (usocket:socket-stream stream))
    (print (read (usocket:socket-stream stream)))
    (usocket:socket-close stream)))


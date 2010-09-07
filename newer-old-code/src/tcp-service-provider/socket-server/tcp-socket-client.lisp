(in-package :sykosomatic.socket-server)

(defgeneric socket-client-socket (client))
(defgeneric socket-client-remote-name (client))
(defgeneric socket-client-remote-port (client))
(defgeneric socket-client-server (client))
(defgeneric disconnect-socket-client (client &rest events))

(defgeneric buffer-client-max-buffer-bytes (client))
(defaccessor buffer-client-input-buffer client)
(defaccessor buffer-client-input-buffer-fill client)
(defaccessor buffer-client-output-buffer-queue client)
(defaccessor buffer-client-output-buffer client)
(defaccessor buffer-client-output-byte-count client)
(defaccessor buffer-client-recent-newline-p client)

;;;
;;; Connection
;;;
(defmethod disconnect-socket-client (client &rest events)
  (let ((fd (iolib:socket-os-fd (socket-client-socket client)))
        (event-base (socket-server-event-base (socket-client-server client))))
    (if (not (intersection '(:read :write :error) events))
        (iolib:remove-fd-handlers event-base fd
                                  :read t :write t :error t)
        (progn
          (when (member :read events)
            (iolib:remove-fd-handlers event-base fd :read t))
          (when (member :write events)
            (iolib:remove-fd-handlers event-base fd :write t))
          (when (member :error events)
            (iolib:remove-fd-handlers event-base fd :error t)))))
  (when (member :close events)
    (close (socket-client-socket client) :abort t)
    (detach-client (socket-client-server client) client)))

;;;
;;; Input
;;;
(defgeneric on-client-read (client)
  (:method (client)
    (handler-case
        (let ((bytes-read
               (nth-value 1 (iolib:receive-from (socket-client-socket client)
                                                :buffer (buffer-client-input-buffer client)
                                                :start (buffer-client-input-buffer-fill client)
                                                :end (1- (buffer-client-max-buffer-bytes client))))))
          (when (zerop bytes-read)
            (error 'end-of-file))
          (incf (buffer-client-input-buffer-fill client) bytes-read))
      (iolib:socket-connection-reset-error ()
        ;; Should do something here
        (format t "Got a reset from ~A.~%" client)
        (finish-output)
        (disconnect-socket-client client :close))
      (end-of-file ()
        ;; Should do something here, too.
        (format t "Received unexpected EOF from ~A.~%" client)
        (finish-output)
        (disconnect-socket-client client :close)))))

(defun ensure-simple-string (string)
  (cond ((simple-string-p string)
         string)
        ((stringp string)
         (aprog1 (make-string (length string) :element-type 'character)
           (map-into it #'identity string)))
        (t (error "Not a string: ~A" string))))

(defgeneric on-special-code (client code)
  (:method (client code)
    (declare (ignore client code))
    (values)))

(defgeneric read-line-from-client (client)
  (:method (client)
    (let ((buffer (buffer-client-input-buffer client))
          (buffer-fill (buffer-client-input-buffer-fill client)))
      (when (plusp buffer-fill)
        ;; We've got input! Try to convert it to a string.
        (flex:with-input-from-sequence (s buffer :end buffer-fill)
          (setf s (flex:make-flexi-stream s :external-format :utf-8))
          (aprog1 (loop with string = (make-array 16 :element-type 'character
                                                  :adjustable t :fill-pointer 0)
                     for char = (handler-case (read-char s nil nil)
                                  (flex:external-format-encoding-error ()
                                    (read-byte s nil nil)))
                     do (cond ((and (characterp char) (not (char= #\newline char)))
                               (vector-push-extend char string))
                              ((and (or (null char)
                                        (char= #\newline)))
                               (return (ensure-simple-string string)))
                              ((numberp char)
                               (on-special-code client char))
                              (t
                               (return nil))))
            (if (flex:peek-byte s nil nil nil)
                ;; If there's still stuff in the buffer, we'll have to shift things to the left.
                (loop
                   for next-byte = (read-byte s nil nil)
                   for i from 0
                   while next-byte
                   do (setf (aref buffer i) next-byte)
                   finally (setf (buffer-client-input-buffer-fill client) (1+ i)))
                (setf (buffer-client-input-buffer-fill client) 0))))))))

;;;
;;; Output
;;;
(defgeneric on-client-write (client)
  (:method (client)
    (handler-case
        (progn
          (unless (or (buffer-client-output-buffer client)
                      (queue-empty-p (buffer-client-output-buffer-queue client)))
            (setf (buffer-client-output-buffer client) 
                  (dequeue (buffer-client-output-buffer-queue client))))
          (when (buffer-client-output-buffer client)
            (let* ((buffer (buffer-client-output-buffer client))
                   (bytes-written (iolib:send-to (socket-client-socket client) buffer
                                                 :start 0
                                                 :end (length buffer))))
              (incf (buffer-client-output-byte-count client)
                    bytes-written)
              (when (= (buffer-client-output-byte-count client)
                       (length buffer))
                (setf (buffer-client-output-buffer client) nil
                      (buffer-client-output-byte-count client) 0)))))
      (iolib:socket-connection-reset-error ()
        (format t "Got a reset from ~A.~%" client)
        (finish-output)
        (disconnect-socket-client client :close))
      (isys:ewouldblock ()
        (format t "Got an EWOULDBLOCK while writing to ~A.~%" client)
        (finish-output))
      (isys:epipe ()
        (format t "Got a hangup while writing to ~A.~%" client)
        (disconnect-socket-client client :close)))))

(defgeneric write-to-client (client data)
  (:method (client (data string))
    (enqueue (flex:string-to-octets data :external-format :utf-8)
             (buffer-client-output-buffer-queue client))))

;;;
;;; TCP Socket Client class
;;;
(defclass tcp-socket-client (fundamental-character-output-stream
                             fundamental-character-input-stream)
  ((socket :reader socket-client-socket :initarg :socket)
   (remote-name :initarg :remote-name :reader socket-client-remote-name)
   (port :initarg :remote-port :reader socket-client-remote-port)
   (max-buffer-bytes :initarg :max-buffer-bytes :initform 16384
                     :reader buffer-client-max-buffer-bytes)
   (input-buffer :accessor buffer-client-input-buffer)
   (input-buffer-fill :initform 0 :accessor buffer-client-input-buffer-fill)
   (output-buffer-queue :accessor buffer-client-output-buffer-queue :initform (make-queue))
   (output-buffer :accessor buffer-client-output-buffer :initform nil)
   (output-byte-count :accessor buffer-client-output-byte-count :initform 0)
   (recent-newline-p :accessor buffer-client-recent-newline-p :initform t)
   (server :initarg :server :reader socket-client-server)))

(defmethod initialize-instance :after ((client tcp-socket-client) &key)
  (multiple-value-bind (name port)
      (iolib:remote-name (socket-client-socket client))
    (setf (slot-value client 'remote-name) name
          (slot-value client 'port) port
          (buffer-client-input-buffer client) (make-array (buffer-client-max-buffer-bytes client)
                                                          :element-type '(unsigned-byte 8)))))

;;;
;;; Gray streams protocol implementation
;;;

;;; Basic
(defmethod close ((client tcp-socket-client) &key abort)
  (close (socket-client-socket client) :abort abort))
(defmethod open-stream-p ((client tcp-socket-client))
  (open-stream-p (socket-client-socket client)))

;; Input
(defmethod stream-read-line ((client tcp-socket-client))
  (read-line-from-client client))
(defmethod stream-clear-input ((client tcp-socket-client))
  (setf (buffer-client-input-buffer-fill client) 0))

;; Output
(defmethod stream-write-char ((client tcp-socket-client) char)
  (write-to-client client (princ-to-string char))
  (setf (buffer-client-recent-newline-p client)
        (eq #\newline char))
  char)
(defmethod stream-line-column ((client tcp-socket-client))
  nil)
(defmethod stream-start-line-p ((client tcp-socket-client))
  (when (buffer-client-recent-newline-p client)
    t))
(defmethod stream-fresh-line ((client tcp-socket-client))
  (unless (stream-start-line-p client)
    (stream-write-char client #\newline)))
(defmethod stream-write-string ((client tcp-socket-client) seq &optional start end)
  (let ((seq (if start (subseq seq start end) seq)))
    (when (plusp (length seq))
      (prog1 (write-to-client client seq)
        (setf (buffer-client-recent-newline-p client)
              (when (eql #\newline (elt seq (1- (length seq)))) t))))))
(defmethod stream-clear-output ((client tcp-socket-client))
  (setf (buffer-client-output-buffer client) nil
        (buffer-client-output-byte-count client) 0))

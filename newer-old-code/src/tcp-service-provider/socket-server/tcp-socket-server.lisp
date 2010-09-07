(in-package :sykosomatic.socket-server)

(defgeneric socket-server-event-base (socket-server))
(defgeneric (setf socket-server-event-base) (new-value socket-server))
(defgeneric socket-server-listen-ip (socket-server))
(defgeneric socket-server-listen-port (socket-server))
(defgeneric socket-server-listener-socket (socket-server))
(defgeneric (setf socket-server-listener-socket) (new-value socket-server))
(defgeneric socket-server-clients (socket-server))

(defgeneric socket-server-client-table (socket-server))
(defgeneric (setf socket-server-client-table) (new-value socket-server))

(defgeneric make-socket-client (socket-server client-socket))

(defgeneric attach-client (server client))
(defgeneric detach-client (server client))

(defgeneric on-client-connection (server))

(defgeneric start-socket-server (server))
(defgeneric update-socket-server (server))
(defgeneric stop-socket-server (server))

;;;
;;; Handling clients
;;;
(defun handle-client-connection (server)
  (let ((client-socket (iolib:accept-connection (socket-server-listener-socket server))))
    (when client-socket
      (let ((client (make-socket-client server client-socket)))
        (format t "~&~A Connected~%" client)
        (attach-client server client)
        (iolib:set-io-handler (socket-server-event-base server)
                              (iolib:socket-os-fd client-socket)
                              :read
                              (lambda (&rest rest)
                                (declare (ignore rest))
                                (on-client-read client)))
        (iolib:set-io-handler (socket-server-event-base server)
                              (iolib:socket-os-fd client-socket)
                              :write
                              (lambda (&rest rest)
                                (declare (ignore rest))
                                (on-client-write client)))))))

;;;
;;; TCP Socket Server
;;;
(defclass tcp-socket-server ()
  ((event-base :initform nil :accessor socket-server-event-base)
   (listen-ip :initform iolib:+ipv4-unspecified+ :initarg :listen-ip :reader socket-server-listen-ip)
   (port :reader socket-server-listen-port :initarg :port)
   (client-table :initform (make-hash-table :test 'equalp) :accessor socket-server-client-table)
   (socket :initform nil :accessor socket-server-listener-socket)))

(defgeneric dispatch-events (server)
  (:method ((sp tcp-socket-server))
    (handler-case
        (iolib:event-dispatch (socket-server-event-base sp) :timeout 0)
      (iolib:socket-connection-reset-error ()
        (format t "Unexpected connection reset.~%"))
      (iolib:hangup ()
        (format t "Unexpected hangup.~%"))
      (end-of-file ()
        (format t "Unexpected EOF.~%")))))

(defmethod make-socket-client ((server tcp-socket-server) client-socket)
  (make-instance 'tcp-socket-client :socket client-socket :server server))

(defmethod attach-client ((server tcp-socket-server) client)
  (setf (gethash `(,(socket-client-remote-name client) ,(socket-client-remote-port client))
                 (socket-server-client-table server))
        client))

(defmethod detach-client ((server tcp-socket-server) client)
  (remhash `(,(socket-client-remote-name client) ,(socket-client-remote-port client))
           (socket-server-client-table server))
  client)

(defmethod socket-server-clients ((server tcp-socket-server))
  (let (clients)
    (maphash (lambda (k client) (declare (ignore k))
                     (push client clients))
             (socket-server-client-table server))
    clients))

(defmethod on-client-connection ((server tcp-socket-server))
  (handle-client-connection server))

(defmethod start-socket-server ((server tcp-socket-server))
  (setf (socket-server-client-table server) (make-hash-table :test 'equalp)
        (socket-server-event-base server) (make-instance 'iolib:event-base))
  (let ((socket (iolib:make-socket :connect :passive
                                   :address-family :internet
                                   :type :stream
                                   :ipv6 nil)))
    (iolib:bind-address socket (socket-server-listen-ip server)
                        :port (socket-server-listen-port server))
    (iolib:listen-on socket :backlog 5)
    (iolib:set-io-handler (socket-server-event-base server)
                          (iolib:socket-os-fd socket)
                          :read
                          (lambda (&rest rest)
                            (declare (ignore rest))
                            (on-client-connection server)))
    (setf (socket-server-listener-socket server) socket))
  server)

(defmethod update-socket-server ((server tcp-socket-server))
  (dispatch-events server)
  server)

(defmethod stop-socket-server ((server tcp-socket-server))
  (with-accessors ((event-base socket-server-event-base) (socket socket-server-listener-socket))
      server
    (disconnect-all-clients server)
    (when event-base
      (close event-base)
      (setf event-base nil))
    (when socket
      (close socket)
      (setf socket nil)))
  server)

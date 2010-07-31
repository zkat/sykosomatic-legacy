;; Copyright 2008-2010 Kat Marchán

;; This file is part of sykosomatic

;; sykosomatic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sykosomatic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with sykosomatic.  If not, see <http://www.gnu.org/licenses/>.

;; tcp-service-provider.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Queue util
;;;
(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun queue-empty-p (queue)
  (null (car queue)))

;;;
;;; Config variables
;;;
(defparameter *server-listen-ip* iolib:+ipv4-unspecified+)
(defparameter *server-port* 4000)

;;;
;;; TCP Clients
;;;

(defclass tcp-client ()
  ((account :initform nil :accessor account)
   (avatar :initform nil :accessor avatar)
   (socket :accessor socket :initarg :socket
           :initform (error "Must provide a socket for this client."))
   (service-provider :accessor service-provider :initarg :provider
                     :initform (error "Must provide a provider for this client."))
   (remote-name :accessor remote-name)
   (port :accessor port)
   (max-buffer-bytes :reader max-buffer-bytes :initarg :max-buffer-bytes :initform 16384)
   (input-buffer :accessor input-buffer)
   (input-buffer-fill :initform 0 :accessor input-buffer-fill)
   (output-buffer-queue :accessor output-buffer-queue :initform (make-queue))
   (output-buffer :accessor output-buffer :initform nil)
   (output-byte-count :accessor output-byte-count :initform 0)))

(defmethod initialize-instance :after ((client tcp-client) &key)
  (multiple-value-bind (name port)
      (iolib:remote-name (socket client))
    (setf (remote-name client) name
          (port client) port
          (input-buffer client) (make-array (max-buffer-bytes client)
                                            :element-type '(unsigned-byte 8)))))

(defmethod print-object ((client tcp-client) s)
  (print-unreadable-object (client s :type t)
    (format s "~A:~A" (remote-name client) (port client))))

(defgeneric disconnect (client &rest events)
  (:method ((client tcp-client) &rest events)
    (let ((fd (iolib:socket-os-fd (socket client)))
          (event-base (event-base (service-provider client))))
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
      (close (socket client))
      (detach-client (service-provider client) client))))

(defgeneric on-client-read (client)
  (:method ((client tcp-client))
    (handler-case
        (let ((bytes-read
               (nth-value 1 (iolib:receive-from (socket client)
                                                :buffer (input-buffer client)
                                                :start (input-buffer-fill client)
                                                :end (1- (max-buffer-bytes client))))))
          (when (zerop bytes-read)
            (error 'end-of-file))
          (incf (input-buffer-fill client) bytes-read))
      (iolib:socket-connection-reset-error ()
        ;; Should do something here
        (format t "Got a reset from ~A.~%" client)
        (finish-output)
        (disconnect client :close))
      (end-of-file ()
        ;; Should do something here, too.
        (format t "Received unexpected EOF from ~A.~%" client)
        (finish-output)
        (disconnect client :close)))))

(defgeneric on-client-write (client)
  (:method ((client tcp-client))
    (handler-case
        (progn
          (when (and (not (output-buffer client))
                     (not (queue-empty-p (output-buffer-queue client))))
            (setf (output-buffer client) (dequeue (output-buffer-queue client))))
          (when (output-buffer client)
            (let* ((buffer (output-buffer client))
                   (bytes-written (iolib:send-to (socket client) buffer
                                                 :start 0
                                                 :end (length buffer))))
              (incf (output-byte-count client)
                    bytes-written)
              (when (= (output-byte-count client)
                       (length buffer))
                (setf (output-buffer client) nil
                      (output-byte-count client) 0)))))
      (iolib:socket-connection-reset-error ()
        (format t "Got a reset from ~A.~%" client)
        (finish-output)
        (disconnect client :close))
      (isys:ewouldblock ()
        (format t "Got an EWOULDBLOCK while writing to ~A.~%" client)
        (finish-output))
      (isys:epipe ()
        (format t "Got a hangup while writing to ~A.~%" client)
        (disconnect client :close)))))

(defgeneric write-to-client (client data)
  (:method ((client tcp-client) (data string))
    (let ((array (make-array (length data) :element-type '(unsigned-byte 8))))
      (enqueue (map-into array #'char-code data)
               (output-buffer-queue client)))))

(defgeneric read-line-from-client (client)
  (:method ((client tcp-client))
    (let ((buffer (input-buffer client))
          (buffer-fill (input-buffer-fill client)))
      (when (and (plusp buffer-fill)
                 (find #\newline buffer :end buffer-fill :key #'code-char))
        ;; Note: We _scrap_ the newline.
        (let ((string (make-string (1- buffer-fill))))
          (loop for code across buffer
             for i below (1- buffer-fill)
             do (setf (aref string i) (code-char code)))
          (setf (input-buffer-fill client) 0)
          string)))))

(defgeneric handle-line (client line)
  (:method ((client tcp-client) line)
    (cond ((null (avatar client))
           (setf (avatar client) line)
           (write-to-client client (format nil "You are now identified as ~S.~%" line)))
          ((string-equal line "look")
           (write-to-client client (format nil "You see nothing in particular.~%")))
          (t
           (write-to-client client (format nil "~A says, ~S~%" (avatar client) line))))))

(defmethod update ((client tcp-client))
  (let ((input (read-line-from-client client)))
    (when input (handle-line client input))))

;;;
;;; TCP service
;;;
(defclass tcp-service-provider (service-provider)
  ((event-base :initform nil :accessor event-base :initarg :event-base)
   (listen-ip :initform *server-listen-ip*  :accessor listen-ip)
   (port :initform *server-port* :accessor port :initarg :port)
   (clients :initform (make-hash-table :test 'equalp) :accessor clients)
   (socket :initform nil :accessor socket)))

(defgeneric attach-client (server client)
  (:method ((server tcp-service-provider) (client tcp-client))
    (setf (gethash `(,(remote-name client) ,(port client)) (clients server))
          client)))

(defgeneric detach-client (server client)
  (:method ((server tcp-service-provider) (client tcp-client))
    (remhash `(,(remote-name client) ,(port client)) (clients server))
    client))

(defgeneric disconnect-all-clients (server)
  (:method ((server tcp-service-provider))
    (maphash (lambda (k client) (declare (ignore k))
                     (disconnect client :close))
             (clients server))))

(defmethod init ((server tcp-service-provider))
  (setf (clients server) (make-hash-table :test 'equalp)
        (event-base server) (make-instance 'iolib:event-base))
  (let ((socket (iolib:make-socket :connect :passive
                                   :address-family :internet
                                   :type :stream
                                   :ipv6 nil)))
    (iolib:bind-address socket (listen-ip server)
                        :port (port server))
    (iolib:listen-on socket :backlog 5)
    (iolib:set-io-handler (event-base server)
                          (iolib:socket-os-fd socket)
                          :read
                          (lambda (&rest rest)
                            (declare (ignore rest))
                            (on-client-connection server)))
    (setf (socket server) socket)))

(defmethod teardown ((server tcp-service-provider))
  (with-accessors ((event-base event-base) (socket socket))
      server
    (disconnect-all-clients server)
    (when event-base
      (close event-base)
      (setf event-base nil))
    (when socket
      (close socket)
      (setf socket nil))))

(defgeneric on-client-connection (server)
  (:method ((server tcp-service-provider))
    (let ((client-socket (iolib:accept-connection (socket server))))
      (when client-socket
        (let ((client (make-instance 'tcp-client
                                     :socket client-socket
                                     :provider server)))
          (format t "~A Connected.~%" client)
          (attach-client server client)
          (iolib:set-io-handler (event-base server)
                                (iolib:socket-os-fd client-socket)
                                :read
                                (lambda (&rest rest)
                                  (declare (ignore rest))
                                  (on-client-read client)))
          (iolib:set-io-handler (event-base server)
                                (iolib:socket-os-fd client-socket)
                                :write
                                (lambda (&rest rest)
                                  (declare (ignore rest))
                                  (on-client-write client)))
          (write-to-client client (format nil "~&Hello. Welcome to Sykosomatic.~%"))
          (write-to-client client (format nil "~&Please enter your name: ")))))))

(defgeneric dispatch-events (service-provider)
  (:method ((sp tcp-service-provider))
    (handler-case
        (iolib:event-dispatch (event-base sp) :timeout 0)
      (iolib:socket-connection-reset-error ()
        (format t "Unexpected connection reset.~%"))
      (iolib:hangup ()
        (format t "Unexpected hangup.~%"))
      (end-of-file ()
        (format t "Unexpected EOF.~%")))))

(defmethod update ((server tcp-service-provider))
  (dispatch-events server)
  (maphash (lambda (k client) (declare (ignore k)) (update client)) (clients server)))

;;;
;;; Input handlers
;;;
(defclass input-handler () ())
(defclass intro-handler (input-handler) ())
(defclass login-handler (input-handler) ())
(defclass gameplay-handler (input-handler) ())
(defclass query-handler (input-handler)
  ((gameplay-handler :initarg :gameplay-handler :accessor gameplay-handler)
   (question :initarg :question :accessor question)))
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

;; main.lisp
;;
;; Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage #:sykosomatic
  (:use :cl :alexandria))
(in-package :sykosomatic)

(defgeneric init (obj))
(defgeneric teardown (obj))
(defgeneric update (obj))

(defclass engine ()
  ((service-providers :initform nil :accessor service-providers))
  (:documentation
   "The engine handles all the core logic and interactions. It communicates with
its service-providers through events."))

(defclass service-provider ()
  ()
  (:documentation
   "Service providers handle users. They translate user interactions into events, 
which the associated engine can then handle."))


;;;
;;; TCP service
;;;
(defclass tcp-service-provider (service-provider)
  ((event-base :accessor event-base :initarg :event-base)
   (port :initform 9999 :accessor port :initarg :port)
   (clients :initform nil :accessor clients)
   (socket :accessor socket)))

(defmethod init ((server tcp-service-provider))
  (setf (clients server) nil
        (event-base server) (make-instance 'iolib:event-base))
  (let ((socket (iolib:make-socket :connect :passive
                                   :address-family :internet
                                   :type :stream
                                   :ipv6 nil)))
    (iolib:bind-address socket iolib:+ipv4-unspecified+
                        :port (port server))
    (iolib:listen-on socket :backlog 5)
    (iolib:set-io-handler (event-base server)
                          (iolib:socket-os-fd socket)
                          :read
                          (make-listener-function server))
    (setf (socket server) socket)))

(defmethod teardown ((server tcp-service-provider))
  (with-accessors ((clients clients) (event-base event-base))
      server
    (map nil (rcurry #'disconnect :close) clients)
    (when event-base
      (close event-base)
      (setf event-base nil))
    (close (socket server))))

(defmethod update ((server tcp-service-provider))
  (dispatch-events server))

(defclass client ()
  ((socket :accessor socket :initarg :socket
           :initform (error "Must provide a socket for this client."))
   (service-provider :accessor service-provider :initarg :provider
                     :initform (error "Must provide a provider for this client."))
   (ip-address :accessor ip-address)
   (port :accessor port)
   (max-buffer-bytes :reader max-buffer-bytes :initarg :max-buffer-bytes :initform 16384)
   (input-buffer :accessor input-buffer)
   (input-buffer-fill :initform 0 :accessor input-buffer-fill)
   (output-buffer :accessor output-buffer)
   (output-buffer-fill :initform 0 :accessor output-buffer-fill)))

(defmethod initialize-instance :after ((client client) &key)
  (multiple-value-bind (ip port)
      (iolib:remote-name (socket client))
    (setf (ip-address client) ip
          (port client) port
          (input-buffer client) (make-array (max-buffer-bytes client)
                                            :element-type '(unsigned-byte 8))
          (output-buffer client) (make-array (max-buffer-bytes client)
                                             :element-type '(unsigned-byte 8)))))

(defmethod print-object ((client client) s)
  (print-unreadable-object (client s :type t :identity t)
    (format s "~A:~A" (ip-address client) (port client))))

(defgeneric disconnect (client &rest events)
  (:method ((client client) &rest events)
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
      (deletef (clients (service-provider client))
               client))))

(defun make-listener-function (server)
  (lambda (fd event exception)
    (declare (ignorable fd event exception))
    (let ((client-socket (iolib:accept-connection (socket server))))
      (when client-socket
        (let ((client (make-instance 'client
                                     :socket client-socket
                                     :provider server)))
          (format t "~A Connected.~%" client)
          (push client (clients server))
          (iolib:set-io-handler (event-base server)
                                 (iolib:socket-os-fd client-socket)
                                 :read
                                 (make-reader-function client))
          (iolib:set-io-handler (event-base server)
                                 (iolib:socket-os-fd client-socket)
                                 :write
                                 (make-writer-function client)))))))

(defun make-reader-function (client)
  (lambda (fd event exception)
    (declare (ignorable fd event exception))
    (handler-case
        (let* ((buffer (input-buffer client))
               (bytes-read
                (nth-value 1 (iolib:receive-from (socket client)
                                                 :buffer buffer
                                                 :start (input-buffer-fill client)
                                                 :end (1- (max-buffer-bytes client))))))
          (when (zerop bytes-read)
            (error 'end-of-file))
          (incf (input-buffer-fill client) bytes-read))
      (iolib:socket-connection-reset-error ()
        ;; Should do something here
        (format t "Got a reset from client.~%")
        (finish-output)
        (disconnect client :close))
      (end-of-file ()
        ;; Should do something here, too.
        (format t "Received unexpected EOF from client.~%")
        (finish-output)
        (disconnect client :close)))))

(defun make-writer-function (client)
  (lambda (fd event exception)
    (declare (ignorable fd event exception))
    (unless (zerop (output-buffer-fill client))
      (handler-case
          (let* ((buffer (output-buffer client))
                 (bytes-written (iolib:send-to (socket client) buffer
                                               :start (output-buffer-fill client)
                                               :end (1- (max-buffer-bytes client)))))
            (decf (output-buffer-fill client) bytes-written))
        (iolib:socket-connection-reset-error ()
          (format t "Got a reset from client.~%")
          (finish-output)
          (disconnect client :close))
        (isys:ewouldblock ()
          (format t "Got an EWOULDBLOCK.~%")
          (finish-output))
        (isys:epipe ()
          (format t "Got a hangup on write.~%")
          (disconnect client :close))))))

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

;; TODO
(defgeneric write-to-client (client data))
(defgeneric read-from-client (client))



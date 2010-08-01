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

(declaim (optimize debug))
;;;
;;; TCP service
;;;
(defclass tcp-service-provider (service-provider)
  ((event-base :initform nil :accessor event-base :initarg :event-base)
   (listen-ip :initform *server-listen-ip*  :accessor listen-ip)
   (port :initform *server-port* :accessor port :initarg :port)
   (clients :initform (make-hash-table :test 'equalp) :accessor clients)
   (socket :initform nil :accessor socket)))

;;;
;;; Handling clients
;;;
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
          (init client))))))

;;;
;;; Events
;;;
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

;;;
;;; Init/Update/Teardown
;;;
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

(defmethod update ((server tcp-service-provider))
  (dispatch-events server))

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

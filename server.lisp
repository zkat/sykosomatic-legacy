;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

;; sykosomatic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sykosomatic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sykosomatic.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:sykosomatic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;=========================================== Server ===========================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~ Class ~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defclass <server> ()
  ((socket
    :accessor socket
    :initarg :socket
    :initform nil
    :documentation "Contains the server's usocket-listener.")
   (clients
    :accessor clients
    :initform nil
    :documentation "List of connected clients.")
   (client-list-lock
    :accessor client-list-lock
    :initarg :client-list-lock
    :initform (bordeaux-threads:make-lock "client-list-lock")
    :documentation "Locks access to the clients list.")
   (connection-thread
    :accessor connection-thread
    :initarg :connection-thread
    :documentation "Thread that runs the function to connect new clients.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~ Init/Destruct ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defvar *default-server-address* "0.0.0.0")
(defvar *default-server-port* 4000)
(defvar *server* nil)

(defun start-server (&key (address *default-server-address*) (port *default-server-port*))
  (log-message :SERVER "Starting server...")
  (let* ((socket (usocket:socket-listen address port :reuse-address t :element-type '(unsigned-byte 8)))
	 (server (make-instance '<server>
				:socket socket)))
    (setf *server* server)
    (log-message :SERVER "Creating server connection thread.")
    (setf (connection-thread *server*) 
	  (bordeaux-threads:make-thread 
	   (lambda () (loop (handler-case (connect-new-client)
			      (sb-bsd-sockets:not-connected-error () 
				(log-message :HAX "Hax0r be sappin' mah unconnected socket."))))) 
	   :name "connector-thread"))
    (log-message :SERVER "Server started successfully.")))

(defun remove-all-clients ()
  "Disconnects and removes all clients from the current server."
  (log-message :SERVER "Disposing of clients.")
  (handler-case
      (apply #'disconnect-client (clients *server*))
    (usocket:unknown-error () 
      (log-message :SERVER 
		   "An unknown error happened with USOCKET while trying to close all client sockets."))
    (simple-error () 
      (log-message :SERVER 
		   "Got a simple error in stop-server. Probably has to do with disconnect-client.")))
  (setf (clients *server*) nil)
  (log-message :SERVER "Clients removed."))

(defun destroy-connection-thread ()
  "Destroys the server's connection thread, if it's running."
  (if (and (connection-thread *server*)
	   (bordeaux-threads:thread-alive-p (connection-thread *server*)))
      (progn
	(bordeaux-threads:destroy-thread (connection-thread *server*))
	(log-message :SERVER "Connection thread successfully shut down."))
      (log-message :SERVER "No thread running, skipping...")))

(defun stop-server ()
  "Stops the server, disconnecting everything."
  (if (not *server*)
      (log-message :SERVER "Tried to stop server, but no server running.")
      (progn
	(log-message :SERVER "Stopping server...")
	(write-to-all-clients "~%SERVER IS SHUTTING DOWN. HIT THE DECK!~%")
	(remove-all-clients)	
	(destroy-connection-thread)
	(usocket:socket-close (socket *server*))
	(log-message :SERVER "Server stopped."))))

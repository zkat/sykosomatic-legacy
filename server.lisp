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

;; server.lisp
;;
;; Contains <server> class, which holds the server socket, client list, and connection thread.
;; Also has the start-server and stop-server functions, and supporting functions for those.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Server class
;;;

(defclass <server> ()
  ((stop-server-p
    :accessor stop-server-p
    :initform nil
    :documentation "Is the server stopping?")
   (socket
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
    :initform (bordeaux-threads:make-recursive-lock "client-list-lock")
    :documentation "Locks access to the clients list.")
   (client-cleanup-queue
    :accessor client-cleanup-queue
    :initform (make-empty-queue)
    :documentation "A queue of client threads that need to be killed.")
   (connection-thread
    :accessor connection-thread
    :initarg :connection-thread
    :documentation "Thread that runs the function to connect new clients.")
   (clients-thread
    :accessor clients-thread
    :documentation "Thread that runs the client i/o stuff.")
   (ticks-per-second
    :initform 30
    :accessor ticks-per-second
    :documentation "Ticks define how many times per second client i/o is processed.")
   (last-tick-time
    :initform (get-internal-real-time)
    :accessor last-tick-time))
  (:documentation "Server class that contains the listener socket, the current list of clients,
the i/o processing ticks, and related slots."))


;;;
;;; Init/Destruct
;;;

(defvar *default-server-address* "localhost")
(defvar *default-server-port* 4000)
(defvar *server* nil)

;;; Clients thread

(defun make-clients-thread (server)
  "Returns a lambda that can be popped into a thread. The function loops through all the clients
connected to *server* and handles their input once per tick. Stops with stop-server-p in <server>."
  (lambda ()
    (loop
       until (stop-server-p server)
       do
	 (bordeaux-threads:with-recursive-lock-held ((client-list-lock server))
	   (loop for client in (clients server)
		 for stream = (usocket:socket-stream (socket client))
	      do
		(handler-case 
		    (progn
		      (when (and (open-stream-p stream)
				 (listen stream))
			(update-activity client)
			(maybe-read-line-from-client client))
		      (funcall (client-step client)))
		  (client-disconnected-error ()
		    (progn
		      (log-message :CLIENT "Client disconnected: ~a" (ip client))
		      (remove-client client))))))
	 (let ((next-tick (+ (last-tick-time server)
			     (/ internal-time-units-per-second (ticks-per-second server))))
	       (now (get-internal-real-time)))
	   (setf (last-tick-time server) now)
	   (when (> next-tick now)
	     (sleep (/ (- next-tick now)
		       internal-time-units-per-second)))))))

;;; Start

(defun start-server (&key (address *default-server-address*) (port *default-server-port*))
  "Takes care of starting up the server."
  (log-message :SERVER "Starting server...")
  (let* ((socket (usocket:socket-listen address port :reuse-address t :element-type '(unsigned-byte 8))) 
	 (server (make-instance '<server>
				:socket socket)))
    (setf *server* server)
    (log-message :SERVER "Creating server connection thread.")
    (setf (connection-thread *server*)
	  (bordeaux-threads:make-thread
	   (lambda () (loop 
			 (handler-case (connect-new-client)
			   (sb-bsd-sockets:not-connected-error ()
			     (log-message :HAX "Got a not-connected-error.")))))
	   :name "sykosomatic-server-connection-thread"))
    (setf (clients-thread *server*)
	  (bordeaux-threads:make-thread (make-clients-thread *server*)))
    (log-message :SERVER "Server started successfully.")))

;;; Stop

(defun stop-server ()
  "Stops the server, disconnecting everything."
  (if (not *server*)
      (log-message :SERVER "Tried to stop server, but no server running.")
      (progn
	(setf (stop-server-p *server*) t)
	(log-message :SERVER "Stopping server...")
	(remove-all-clients)
	(destroy-connection-thread)
	(usocket:socket-close (socket *server*))
	(setf *server* nil)
	(log-message :SERVER "Server stopped."))))

(defun remove-all-clients ()
  "Disconnects and removes all clients from the current server."
  (if (clients *server*)
      (progn
	(log-message :SERVER "Disposing of clients.")
	(mapcar #'remove-client (clients *server*))
	(log-message :SERVER "Clients removed."))
      (log-message :SERVER "No clients to remove. Skipping client removal.")))

(defun destroy-connection-thread ()
  "Destroys the server's connection thread, if it's running."
  (if (and (connection-thread *server*)
	   (bordeaux-threads:thread-alive-p (connection-thread *server*)))
      (progn
	(bordeaux-threads:destroy-thread (connection-thread *server*))
	(log-message :SERVER "Connection thread successfully shut down."))
      (log-message :SERVER "No thread running, skipping thread destruction...")))

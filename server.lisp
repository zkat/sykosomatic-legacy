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
    :accessor server-socket
    :initarg :server-socket)
   (clients
    :accessor clients
    :initform nil)
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
(defvar *current-server* nil)

(defun start-server (&key (address *default-server-address*) (port *default-server-port*))
  (format t "Starting server...~%")
  (let* ((socket (usocket:socket-listen address port :reuse-address t :element-type '(unsigned-byte 8)))
	 (server (make-instance '<server>
				:server-socket socket)))
    (setf *current-server* server)
    (format t "Creating server connection thread.~%")
    (setf (connection-thread *current-server*) 
	  (bordeaux-threads:make-thread (lambda () (loop (connect-new-client))) :name "connector-thread"))
    (format t "Server started successfully.~%")))

(defun stop-server ()
  (if (not *current-server*)
      (format t "No server running.")
      (progn
	(loop for client in (clients *current-server*)
	     do (usocket:socket-close (socket client)))
	(setf (clients *current-server*) nil)
	(if (and (connection-thread *current-server*)
		 (bordeaux-threads:thread-alive-p (connection-thread *current-server*)))
	    (bordeaux-threads:destroy-thread (connection-thread *current-server*))
	    (format t "No thread running, skipping...~%"))
	(usocket:socket-close (server-socket *current-server*))
	(format t "Server stopped."))))
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

;; client.lisp
;;
;; Contains the <client> class, and the client handling code, which includes connecting and
;; disconnecting from the server, handling input/output for clients, and running the client
;; main function (which runs in a thread). There's also some stress-test code at the bottom.
;;
(in-package :sykosomatic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;========================================== Client ============================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; NOTE: This whole thing should be ported to use asynchronous i/o. This'll probably happen after prototyping.
;;       The changes shouldn't be too big. The thread should be removed from <client>, the read and write
;;       functions should be fixed up, and so should connect-new-client. That -should- be all that needs fixed.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Class ~~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defclass <client> ()
  ((socket
    :initarg :socket
    :accessor socket
    :initform nil
    :documentation "Socket belonging to this client.")
   (thread
    :initarg :thread
    :accessor thread
    :initform nil
    :documentation "Thread where everything related to this client resides.")
   (ip
    :initarg :ip
    :reader ip
    :initform nil
    :documentation "Client's IP address.")
   (last-active
    :initarg :last-active
    :initform (get-universal-time)
    :accessor last-active
    :documentation "Time when last input was received from client.")
   (account
    :initarg :account
    :accessor account
    :initform nil
    :documentation "The account associated with this session.")
   (avatar
    :initarg :avatar
    :accessor avatar
    :initform nil
    :documentation "The character linked to this client session.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~ Connection ~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define-condition client-disconnected-error (error)
  ((text :initarg :text :reader text)))

(defun connect-new-client ()
  "Connects a new client to the main server."
  (let ((socket (usocket:socket-accept (socket *server*))))
    (let ((client (make-instance '<client>
				 :socket socket
				 :ip (usocket:get-peer-address socket))))
      (log-message :CLIENT (format nil "New client: ~a" (ip client)))
      (setf (thread client) 
	    (bordeaux-threads:make-thread 
	     (lambda () (handler-case 
			    (client-main client)
			  (client-disconnected-error ()
			    (progn
			       (log-message :CLIENT "Client disconnected. Terminating.")
			       (remove-client client))))
			       :name "sykosomatic-client-main-thread")))
      (bordeaux-threads:with-lock-held ((client-list-lock *server*))
	(push client (clients *server*))))))

;; FIXME: This is working from within the client-thread. Meaning: It can't destroy the thread.
(defun disconnect-client (client)
  "Disconnects the client and removes it from the current clients list."
  (with-accessors ((socket socket)) client
    (if socket
	(usocket:socket-close socket) ;;shutdown procedure should probably go here.
	(log-message :CLIENT
		     (format nil "Tried disconnecting client ~a, but nothing to disconnect." (ip client))))))

(defun remove-client (client)
  "Removes client from the server's client-list."
  (disconnect-client client)
  (bordeaux-threads:with-lock-held ((client-list-lock *server*)) 
    (setf (clients *server*)
	  (remove client (clients *server*)))))
  
(defun client-idle-time (client)
  "How long, in seconds, since activity was last received from client."
  (- (get-universal-time) (last-active client)))

(defun update-activity (client)
  "Updates the activity time of client to now."
  (with-accessors ((activity last-active)) client
    (setf activity (get-universal-time))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Client I/O ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;
;;        Input       ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun read-line-from-client (client)
  "Grabs a line of input from a client. Takes care of stripping out any unwanted bytes.
Throws a CLIENT-DISCONNECTED-ERROR if it receives an EOF."
  (handler-case
      (let* ((stream (usocket:socket-stream (socket client)))
	     (collected-bytes (loop for b = (read-byte stream)
				 until (= b (char-code #\Newline))
				 unless (not (standard-char-p (code-char b)))
				 collect (code-char b))))
	(update-activity client)
	(coerce collected-bytes 'string))
    (end-of-file ()
      (error 'client-disconnected-error :text "End-of-file. Stream disconnected remotely."))
    (simple-error () (error 'client-disconnected-error 
			    :text "Got a simple error while trying to write to client.
Assuming disconnection."))))

(defun prompt-client (client format-string &rest format-args)
  "Prompts a client for input"
  (write-to-client client format-string format-args)
  (read-line-from-client client))

(defun client-y-or-n-p (client string)
  "y-or-n-p that sends the question over to the client."
  (write-to-client client string)
  (let ((answer (prompt-client client "(y or n)")))
    (cond ((string-equal "y" (char answer 0))
	   t)
	  ((string-equal "n" (char answer 0))
	   nil)
	  (t
	   (progn
	     (write-to-client client "Please answer y or n.~%")
	     (client-y-or-n-p client string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Output       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun write-to-all-clients (format-string &rest format-args)
  "Sends a given string to all connected clients."
  (with-accessors ((clients clients)) *server*
     (mapcar #'(lambda (client) (apply #'write-to-client client format-string format-args)) clients)))

(defun write-to-client (client format-string &rest format-args)
  "Sends a given STRING to a particular client."
  (let ((string (apply #'format nil format-string format-args)))
    (if (socket client)
	(handler-case
	    (let* ((stream (usocket:socket-stream (socket client)))
		   (bytes (loop for char across string
			     collect (char-code char))))
	      (loop for byte in bytes
		 do (write-byte byte stream)
		 finally (finish-output stream)))
	  (sb-int:simple-stream-error () (error 'client-disconnected-error 
						:text "Broken pipe. Can't write to client."))
	  (simple-error () (error 'client-disconnected-error 
				  :text "Got a simple error while trying to write to client. Assuming disconnection.")))
	(error 'client-disconnected-error 
	       :text "Can't write to client. There's no socket to write to."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~~ Main ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun client-main (client)
  "Main function for clients."
;;Keep it simple at first. Grab input, echo something back.
;; Later on, allow clients to enter players, and run in the main player loop.
;; Then start getting fancy from there.
  (write-to-client client "Hello, welcome to SykoSoMaTIC~%")
  (loop
     (client-echo-ast client)))

;; Temporary
(defun client-echo-input (client)
  (let ((input (prompt-client client "~~> ")))
    (if (string-equal input "quit")
	(disconnect-client client)
	(write-to-client client "You wrote: ~a~%~%" input))))

(defun client-echo-ast (client)
  (let ((input (prompt-client client "~~> ")))
    (if (string-equal input "quit")
	(disconnect-client client)
	(write-to-client client "Parsed AST: ~a~%~%" (parse-string input)))))

(defun player-main-loop (client)
  "Main function for playing a character. Subprocedure of client-main"
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~ Stress-test ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defvar *test-clients* nil)

(defclass <test-client> ()
  ((thread
    :accessor thread
    :initarg :thread)
   (socket
    :accessor socket
    :initarg :socket)))

(defun spam-server-with-lots-of-clients (num-clients)
  (dotimes (i num-clients)
    (push (make-and-run-test-client) *test-clients*)))

(defun kill-the-infidels ()
  (loop
     for client in *test-clients*
     do (progn
	  (bordeaux-threads:destroy-thread (thread client))
	  (usocket:socket-close (socket client)))))

(defun make-and-run-test-client ()
  (let ((test-client (make-instance '<test-client>
			:socket (usocket:socket-connect 
				 "dagon.ath.cx" 4000 
				 :element-type '(unsigned-byte 8)))))
    (setf (thread test-client)
	  (bordeaux-threads:make-thread (lambda ()
					  (spam-loop test-client))
					:name "sykosomatic-test-client-thread"))
    test-client))

(defun spam-loop (client)
  (loop
     (write-to-client client "lucy and the sky with diamonds lalalalalalallalalalalal")
     (sleep 0.5)))

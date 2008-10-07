;; Copyright 2008 Kat Marchan

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

;; client.lisp
;;
;; Contains the <client> class, and the client handling code, which includes connecting and
;; disconnecting from the server, handling input/output for clients, and running the client
;; main function (which runs in a thread). There's also some stress-test code at the bottom.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :org.sykosomatic.network)

;; Note: After much consideration, it turns out this is the best approach. Async i/o is too
;;       much trouble to bother with, and can cause a bunch of its own problems.

;;;
;;; Client Class
;;;

(defclass <client> ()
  ((socket
    :initarg :socket
    :accessor socket
    :initform nil
    :documentation "Socket belonging to this client.")
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
   (client-continuation
    :initform nil
    :accessor client-continuation)
   (client-step
    :initform nil
    :accessor client-step)
   (partial-line
    :initform nil
    :accessor partial-line
    :documentation "Holds a line that's currently being read, while reader receives a newline.")
   (read-lines
    :initform (make-empty-queue)
    :accessor read-lines
    :documentation "A queue of lines that have been read in from the client."))
  (:documentation "Contains basic information about the current client like the connected socket,
the client's IP address, last activity time, associated account (if any), and associated avatar (if 
any). Also contains several slots that handle asynchronous client i/o."))

;;;
;;; Connection
;;;

(defun format-ip (ip)
  "Converts an IP in array format into a string."
  (format nil "~{~d~^.~}" (loop for ip-part across ip
				 collect ip-part)))

(defun connect-new-client ()
  "Connects a new client to the main server."
  (let ((socket (usocket:socket-accept (socket *server*))))
    (let ((client (make-instance '<client> 
				 :socket socket
				 :ip (format-ip (usocket:get-peer-address socket)))))
      (client-init client)
      (log-message :CLIENT "New client: ~a" (ip client))
      (bordeaux-threads:with-lock-held ((client-list-lock *server*))
	(push client (clients *server*))))))

(defun disconnect-client (client)
  "Disconnects the client and removes it from the current clients list."
  (with-accessors ((socket socket)) client
    (if socket
	(progn
	  (usocket:socket-close socket))
	(log-message :CLIENT 
		     "Tried disconnecting client ~a, but nothing to disconnect." (ip client)))))

(defun remove-client (client)
  "Removes client from the server's client-list."
  (disconnect-client client)
  (bordeaux-threads:with-recursive-lock-held ((client-list-lock *server*)) 
    (setf (clients *server*)
	  (remove client (clients *server*)))))
  
(defun client-idle-time (client)
  "How long, in seconds, since activity was last received from client."
  (- (get-universal-time) (last-active client)))

(defun update-activity (client)
  "Updates the activity time of client to now."
  (with-accessors ((activity last-active)) client
    (setf activity (get-universal-time))))

;;; Conditions

(define-condition client-disconnected-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Called whenever it's assumed that the client is disconnected."))

;;;
;;; Client i/o
;;;

;;; Input

(defun maybe-read-line-from-client (client)
  "Grabs a line of input from a client. Takes care of stripping out any unwanted bytes.
Throws a CLIENT-DISCONNECTED-ERROR if it receives an EOF."
  (handler-case
      (let ((stream (usocket:socket-stream (socket client))))
	(loop for b = (when (listen stream) 
			(read-byte stream))
	   do
	   (cond ((null b)
		  (return))
		 ((= b #.(char-code #\Newline))
		  (enqueue (read-lines client) (coerce
						(nreverse (partial-line client))
						'string))
		  (setf (partial-line client) nil)
		  (return))
		 ((standard-char-p (code-char b))
		  (push (code-char b) (partial-line client))))))
    (end-of-file ()
      (error 'client-disconnected-error :text "End-of-file. Stream disconnected remotely."))
    (simple-error () (error 'client-disconnected-error 
			    :text "Got a simple error while trying to write to client.
Assuming disconnection."))))

(defun read-line-from-client (client)
  "Reads a single line of input from a client (delimited by a newline)."
  (dequeue (read-lines client)))

(defun/cc prompt-client (client &optional format-string &rest format-args)
  "Continuation used for prompting a client for input."
  (when format-string 
    (write-to-client client format-string format-args))
  (if (queue-empty-p (read-lines client))
      (let/cc k
	(setf (client-continuation client) k))
      (read-line-from-client client)))

;; TODO: This is inconsistent. No reason why it wouldn't accept regular format arguments.
(defun/cc client-y-or-n-p (client string)
  "y-or-n-p that sends the question over to the client."
  (write-to-client client string)
  (let ((answer (prompt-client client " (y or n) ")))
    (cond ((string-equal "y" (char answer 0))
	   t)
	  ((string-equal "n" (char answer 0))
	   nil)
	  (t
	   (progn
	     (write-to-client client "Please answer y or n.~%")
	     (client-y-or-n-p client string))))))

;;; Output

(defun write-to-all-clients (format-string &rest format-args)
  "Sends a given string to all connected clients."
  (with-accessors ((clients clients)) *server*
    (when clients
      (mapcar #'(lambda (client) (apply #'write-to-client client format-string format-args)) clients))))

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
	  (sb-int:closed-stream-error () (error 'client-disconnected-error 
						:text "Stream was closed. Can't write to client."))
	  (simple-error () (error 'client-disconnected-error 
				  :text "Got a simple error while trying to write to client. Assuming disconnection.")))
	(error 'client-disconnected-error 
	       :text "Can't write to client. There's no socket to write to."))))

;;;
;;; Client main
;;;

(defun client-init (client)
  "Initializes a client, and sets the main function to step through."
  (write-to-client client "Hello, welcome to SykoSoMaTIC~%")
  (setf (client-step client) (make-client-step-with-continuations 
			      client
			      #'(lambda (client) (client-main client)))))

(defun/cc client-main (client)
  "Main function to run clients through."
  (funcall *main-client-function* client))

(defun make-client-step-with-continuations (client function)
  "Wrap some CPS transformed function of one argument (client) handling client IO
 with continuation handler."
  (lambda ()
    (if (client-continuation client)
	(unless (queue-empty-p (read-lines client))
	  (let ((client-continuation (client-continuation client)))
	    (setf (client-continuation client) nil)
	    (funcall client-continuation (read-line-from-client client))))
	(funcall function client))))

;;; Testing
;;;

;;; TODO: Move these to a testing area.
;;; dummy mains

(defun/cc client-echo-input (client)
  "Prompts client for input, and echoes back whatever client wrote."
  (let ((input (prompt-client client "~~> ")))
    (if (string-equal input "quit")
	(disconnect-client client)
	(write-to-client client "You wrote: '~a'~%~%" input))))

;; stress test
(defvar *test-clients* nil)

(defclass <test-client> ()
  ((socket
    :accessor socket
    :initarg :socket)
   (client-step :accessor client-step)))

(defun spam-server-with-lots-of-clients (num-clients)
  (dotimes (i num-clients)
    (push (make-and-run-test-client) *test-clients*)))

(defun kill-the-infidels () ;; this breaks the server. keep away!
  (loop
     for client in *test-clients*
     do (progn
	  (usocket:socket-close (socket client)))))

(defun make-and-run-test-client ()
  (let ((test-client (make-instance '<test-client>
			:socket (usocket:socket-connect 
				 "localhost" 4000 
				 :element-type '(unsigned-byte 8)))))
    (setf (client-step test-client)
	  (lambda ()
	    (spam-loop-step test-client)))
    test-client))

(defun spam-loop-step (client)
  (write-to-client client "look with my eyes at the guy with the funny hat 'hahaha noob"))

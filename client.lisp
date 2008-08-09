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

(in-package :sykosomatic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;========================================== Client ============================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Class ~~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defclass <client> ()
  ((socket
    :initarg :socket
    :reader socket
    :documentation "Socket belonging to this client.")
   (thread
    :initarg :thread
    :accessor thread
    :documentation "Thread where everything related to this client resides.")
   (ip
    :initarg :ip
    :reader ip
    :documentation "Client's IP address.")
   (account
    :initarg :account
    :accessor account
    :documentation "The account associated with this session.")
   (avatar
    :initarg :avatar
    :accessor avatar
    :documentation "The character linked to this client session.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~ Functions ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun connect-new-client ()
  "Connects a new client to the main server."
  (let ((socket (usocket:socket-accept (server-socket *current-server*))))
    (let ((client (make-instance '<client>
				 :socket socket
				 :ip (usocket:get-peer-address socket))))
      (log-message :CLIENT (format nil "New client: ~a" (ip client)))
      (send-to-client client (format nil "Hello. Welcome to SykoSoMaTIC.~%"))
      (push client (clients *current-server*)))))

(defun read-line-from-client (client)
  "Grabs a line of input from a client. Takes care of stripping out any unwanted bytes."
  (let* ((stream (usocket:socket-stream (socket client)))
	 (collected-bytes (loop with b
                              do (setf b (read-byte stream))
			     until (= b (char-code #\Newline))
			     unless (not (standard-char-p (code-char b)))
			     collect (code-char b))))
    (coerce collected-bytes 'string)))

(defun write-to-all-clients (string)
  "Sends a given string to all connected clients."
  (loop for client in (clients *current-server*)
       do (send-to-client client string)))

(defun write-to-client (client string)
  "Sends a given STRING to a particular client."
  (let* ((stream (usocket:socket-stream (socket client)))
	 (bytes (loop for char across string
			       collect (char-code char))))
    (loop for byte in bytes
       do (write-byte byte stream)
       finally (finish-output stream))))

(defun prompt-client (client prompt-string)
  "Prompts a client for input"
  (let ((client-stream (usocket:socket-stream (socket client))))
    (format client-stream "~a " prompt-string)
    (force-output client-stream)
    (read-line client-stream)))

(defun client-y-or-n-p (client string)
  "y-or-n-p that sends the question over to the client."
  (send-to-client client string)
  (let ((answer (prompt-client client "(y or n)")))
    (cond ((string-equal "y" (char answer 0))
	   t)
	  ((string-equal "n" (char answer 0))
	   nil)
	  (t
	   (progn
	     (send-to-client client "Please answer y or n.~%")
	     (client-y-or-n-p client string))))))
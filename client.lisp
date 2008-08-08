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

(defclass <account> ()
  ((username
    :initarg :username
    :reader username)
   (password
    :initarg :password
    :accessor password)
   (id
    :initform (incf *account-ids*)
    :reader id
    :documentation "Unique account ID number.")
   (avatars
    :accessor avatars
    :initform nil
    :documentation "Characters belonging to this account.")
   (client
    :accessor client
    :initform nil
    :documentation "Client currently associated with this account.")    
   (known-ips
    :initarg :known-ips
    :accessor know-ips
    :initform nil
    :documentation "All IPs this account has been known no use.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~ Functions ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun connect-new-client ()
  (let ((socket (usocket:socket-accept (server-socket *current-server*))))
    (let ((client (make-instance '<client>
				 :socket socket
				 :ip (usocket:get-peer-address socket))))
      (format t "New client: ~a" (ip client))
      (push client (clients *current-server*)))))
 
(defun get-input-from-client (client)
  (handler-case (read-line (usocket:socket-stream (socket client)))
    (sb-int:stream-decoding-error () (send-to-client client "You sent junk. Try again."))))

(defun broken-get-input-from-client (client)
  (read-line (usocket:socket-stream (socket client))))

(defun send-to-client (client string)
  "Sends a given STRING to a particular client."
  (format (usocket:socket-stream (socket client)) "~a~%" string)
  (force-output (usocket:socket-stream (socket client))))


(defun prompt-client (client prompt-string)
  "Prompts a client for input"
  (let ((client-stream (usocket:socket-stream (socket client))))
    (format client-stream "~a " prompt-string)
    (force-output client-stream)
    (read-line client-stream)))

(defun client-y-or-n-p (client string)
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

(defun login-to-account (client)
  "Logs a user into their account"
  (send-to-client "Welcome to SykoSoMaTIC Beta(tm)."))
  
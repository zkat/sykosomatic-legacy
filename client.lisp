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
   (ip
    :initarg :ip
    :reader ip
    :documentation "Client's IP address.")
   (player
    :initarg :player
    :accessor player
    :documentation "The player object linked to this client.")))

(defclass <account> ()
  ((username
    :initarg :username
    :reader username)
   (password
    :initarg :password
    :accessor password)
   (characters
    :accessor characters
    :initform nil)))    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~ Functions ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defun add-new-client (server socket)
  (let ((client (make-instance '<client>
			       :socket socket
			       :ip (get-peer-address socket))))
    (format t "New client: ~a" (ip client))
    (pushnew client (clients server))))

(defun send-to-client (client string)
  "Sends a given STRING to a particular client."
  (format (socket-stream (socket client)) "~a" string)
  (force-output (socket-stream (socket client))))

(defun prompt-client (client prompt-string)
  "Prompts a client for input"
  (let ((client-stream (socket-stream (socket client))))
    (format client-stream "~a" prompt-string)
    (force-output client-stream)
    (read-line client-stream)))
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
    :initform nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~ Init/Destruct ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defvar *default-server-address* "127.0.0.1")
(defvar *current-server* nil)

(defun start-server (&key (address *default-server-address*) port)
  (let* ((socket (socket-listen address port :reuseaddress t))
	 (server (make-instance '<server>
				:server-socket socket)))
    (setf *current-server* server)))

(defun stop-server ()
  (if (not *current-server*)
      (format t "No server running.")
      (socket-close (server-socket *current-server*))))
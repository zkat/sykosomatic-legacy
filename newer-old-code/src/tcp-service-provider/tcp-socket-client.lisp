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

;; tcp-socket-client.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(declaim (optimize debug))

;;;
;;; Client Protocol Implementation
;;;
(defmethod print-object ((client tcp-socket-client) s)
  (print-unreadable-object (client s :type t)
    (princ (client-name client) s)))

(defmethod client-name ((client tcp-socket-client))
  (format nil "~A:~A"
          (socket-client-remote-name client)
          (socket-client-remote-port client)))

(defmethod client-service-provider ((client tcp-socket-client))
  (socket-client-server client))

(defmethod send-message ((client tcp-socket-client) (message string))
  (format client message))

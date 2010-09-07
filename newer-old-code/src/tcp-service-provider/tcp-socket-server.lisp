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

;; tcp-socket-server.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(declaim (optimize debug))

;;;
;;; Service Provider protocol implementation
;;;
(defmethod start-service-provider ((server tcp-socket-server))
  (start-socket-server server))
(defmethod update-service-provider ((server tcp-socket-server))
  (update-socket-server server))
(defmethod stop-service-provider ((server tcp-socket-server))
  (stop-socket-server server))

(defmethod connected-clients ((server tcp-socket-server))
  (socket-server-clients server))

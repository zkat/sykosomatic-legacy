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

;; tcp-service-provider.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Service Provider protocol implementation
;;;
(defclass tcp-service-provider (tcp-socket-server) ())

(defmethod start-service-provider ((provider tcp-service-provider))
  (start-socket-server provider))

(defmethod update-service-provider ((provider tcp-service-provider))
  (update-socket-server provider))

(defmethod stop-service-provider ((provider tcp-service-provider))
  (stop-socket-server provider))

(defmethod make-socket-client ((provider tcp-service-provider) socket)
  (make-instance 'tcp-client :main (client-main-function (service-provider-engine engine provider))
                 :socket client-socket :server provider))

(defgeneric client-continuation (client))
(defgeneric (setf client-continuation) (continuation client))
(defgeneric client-main-function (client))
(defgeneric (setf client-main-function) (new-function client))
(defgeneric client-soul (client))
(defgeneric (setf client-soul) (soul client))
(defgeneric client-service-provider (client))

(defclass tcp-client (tcp-socket-client)
  ((main :initarg :main :accessor client-main-function)
   (k :accessor client-continuation)
   (soul :accessor client-soul)))

(defmethod on-client-read :after ((client tcp-client))
  (awhen (read-line client)
    (handle-client-line client it)))

(defmethod handle-client-line ((client tcp-client) line)
  (aif (client-continuation client)
       (progn (setf (client-continuation client) nil)
              (funcall it line))
       (funcall (client-main-function client) client line)))

(defmethod client-service-provider ((client tcp-client))
  (socket-client-server client))
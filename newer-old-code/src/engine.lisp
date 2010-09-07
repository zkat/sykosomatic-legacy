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

;; engine.lisp
;;
;; Engine protocol and base implementation, service providers, clients, souls, and bodies.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(declaim (optimize debug))

;;;
;;; Engine API
;;;
(defgeneric start-engine (engine))
(defgeneric update-engine (engine)
  (:method (engine)
    (map nil #'update-service-provider (engine-service-providers engine))))
(defgeneric stop-engine (engine))
(defgeneric engine-service-providers (engine)
  (:documentation "Returns a sequence of service providers associated with this engine."))

;;;
;;; Soul API
;;;
(defgeneric soul-name (soul)
  (:documentation "Identifier for SOUL, i.e. IP address + remote port. Used when listing connected
  users."))
(defgeneric soul-account (soul)
  (:documentation "Account currently associated with SOUL."))
(defgeneric soul-body (soul)
  (:documentation "Body SOUL is currently controlling."))
(defgeneric soul-service-provider (soul))
(defgeneric disconnect-soul (soul)
  (:documentation "Immediately disconnects SOUL from its service provider."))
(defgeneric send-message (soul message))

;;;
;;; Service provider API
;;;
(defgeneric service-provider-engine (service-provider)
  (:documentation "Returns the engine currently associated with SERVICE-PROVIDER."))
(defgeneric start-service-provider (service-provider)
  (:documentation "Initializes and runs SERVICE-PROVIDER. This function must not block."))
(defgeneric update-service-provider (service-provider))
(defgeneric stop-service-provider (service-provider))
(defgeneric service-provider-engine (service-provider))
(defgeneric connected-souls (service-provider))
(defgeneric disconnect-all-souls (service-provider))

;;;
;;; Body protocol
;;;

;;; Base protocol - All bodies must implement this. Bodies are inhabitable objects
;;;                 that can be 'possessed' by souls.
(defgeneric body-souls (body)
  (:documentation "Returns a sequence of souls currently attached to BODY."))

;;; Location protocol - Bodies that can be placed in locations must implement this.
(defgeneric body-location (body))
(defgeneric (setf body-location) (new-location body))

;;; Name protocol
(defgeneric body-name (body))
(defgeneric body-description (body))

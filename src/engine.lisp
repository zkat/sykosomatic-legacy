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
;; Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(declaim (optimize debug))

;;;
;;; Engine API
;;;
(defgeneric init (obj))
(defgeneric teardown (obj))
(defgeneric update (obj))
(defgeneric run (obj))

(defclass engine ()
  ((service-providers :initform nil :accessor service-providers
                      :initarg :providers)
   (rooms :initform nil :accessor rooms)
   (newbie-room :initarg :newbie-room :accessor newbie-room))
  (:documentation
   "The engine handles all the core logic and interactions. It communicates with
its service-providers through events."))

(defmethod init ((engine engine))
  nil)
(defmethod teardown ((engine engine))
  nil)
(defmethod update ((engine engine))
  nil)
(defmethod init :before ((engine engine))
  (map nil #'init (service-providers engine)))
(defmethod teardown :before ((engine engine))
  (map nil #'teardown (service-providers engine)))
(defmethod update :before ((engine engine))
  (map nil #'update (service-providers engine)))

(defmethod run ((engine engine))
  (unwind-protect
       (progn
         (init engine)
         (loop (update engine)))
    (teardown engine)))

(defclass soul ()
  ((account :initform nil :initarg :account :accessor account)
   (body :initform nil :accessor body)
   (client :initarg :client :accessor client)))

(defclass client ()
  ((soul :accessor soul :initform nil)
   (service-provider :accessor service-provider
                     :initarg :provider
                     :initform (error "Must provide a provider for this client."))))

(defclass body (document) ())

(def-doc-accessors body
  (name "name")
  (description "description")
  (location "location"))

(defun make-body (name description)
  (let ((uuid (gen-uuid)))
    (put-document *db* uuid
                  (mkhash "type" "body"
                          "name" name
                          "description" description
                          "location" nil))
    (make-instance 'body :document (get-document *db* uuid))))

(defparameter *body-id->soul* (make-hash-table :test #'equal))

(defun body-id->soul (body-id)
  (hashget *body-id->soul* body-id))

(defun body->soul (body)
  (body-id->soul (uuid body)))

(defun (setf body->soul) (soul body)
  (setf (hashget *body-id->soul* (uuid body)) soul))

(defmethod teardown ((soul soul))
  (awhen (body soul)
    (remhash (uuid it) *body-id->soul*)))

(defclass service-provider ()
  ()
  (:documentation
   "Service providers handle users. They translate user interactions into events,
which the associated engine can then handle."))

(defgeneric handle-player-command (player input))

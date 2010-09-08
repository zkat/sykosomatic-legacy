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

;; document.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage #:sykosomatic.db.document
  (:use :cl))
(cl:in-package :sykosomatic.db.document)

;;;
;;; Document protocol
;;;
(defgeneric document-engine (document))
(defgeneric document-db (document))
(defgeneric document-slot-value (document slot-name))
(defgeneric (setf document-slot-value) (new-value document slot-name))
(defgeneric document-uuid (document))
(defgeneric document-revision (document))
(defgeneric document-type (document))
(defgeneric gen-uuids (engine &optional count))
(defgeneric update-document (document))
(defgeneric save-document (document))

;;;
;;; Chillax-based implementation
;;;
(cl:defpackage #:sykosomatic.db.document.chillax
  (:use :cl :sykosomatic.db.document :chillax :chillax.utils))
(cl:in-package :sykosomatic.db.document.chillax)

(defclass document ()
  ((engine :initarg :engine :reader document-engine)
   (db :initarg :db :reader document-db)
   (document :initarg :document)))

(defmethod document-slot-value ((document document) slot-name)
  (at (slot-value document 'document) slot-name))
(defmethod (setf document-slot-value) (new-value (document document) slot-name)
  (setf (at (slot-value document 'document) slot-name) new-value))

(defmethod gen-uuids (db &optional (count 10))
  (at (get-uuids (database-server db) :number count) "uuids"))

(defun gen-uuid (db)
  (car (gen-uuids db 1)))

(defmethod document-uuid ((doc document))
  (document-slot-value doc "_id"))

(defmethod document-revision ((doc document))
  (document-slot-value doc "_rev"))

(defmethod document-type ((doc document))
  (document-slot-value doc "type"))

(defmethod update-document ((doc document))
  (setf (slot-value doc 'document)
        (get-document (document-db doc) (document-uuid doc)))
  doc)

(defmethod save-document ((doc document))
  (put-document (document-db doc) (document-uuid doc) (slot-value doc 'document))
  doc)

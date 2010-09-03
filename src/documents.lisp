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

;; documents.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defparameter *db* (ensure-db (make-instance 'yason-server) "sykosomatic"))

(defclass document ()
  ((%document :initarg :document)
   (%transient :initform (make-hash-table :test #'equal))))

(defun gen-uuids (&optional (count 1))
  (hashget (get-uuids (server *db*) :number count) "uuids"))
(defun gen-uuid ()
  (car (gen-uuids 1)))

(defmacro def-doc-accessor (class slotname stringname &key transientp)
  `(progn
     (defmethod ,slotname ((,class ,class))
       (hashget (slot-value ,class ',(if transientp '%transient '%document)) ,stringname))
     (defmethod (setf ,slotname) (new-value (,class ,class))
       (setf (hashget (slot-value ,class ',(if transientp '%transient '%document))
                      ,stringname) new-value))))

(defmacro def-doc-accessors (class &body body)
  `(progn
     ,@(loop for (slot slotstring . args) in body
         collect `(def-doc-accessor ,class ,slot ,slotstring ,@args))))

(def-doc-accessors document
  (uuid "_id")
  (revision "_rev")
  (type "type"))

(defmethod update ((doc document))
  (setf (slot-value doc '%document)
        (get-document *db* (uuid doc)))
  doc)

(defgeneric save (doc)
  (:method ((doc document))
    (put-document *db* (uuid doc) (slot-value doc '%document))
    doc))


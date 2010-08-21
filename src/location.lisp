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

;; location.lisp
;;
;; Rooms/places/locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defvar *default-location*)

(defclass location (document) ())

(def-doc-accessors location
  (name "name")
  (description "description")
  (contents "contents"))

(defun make-location (description)
  (let ((uuid (gen-uuid)))
    (put-document *db* uuid
                  (mkhash "type" "location"
                          "name" "location"
                          "description" description
                          "contents" #()))
    (make-instance 'location :document (get-document *db* uuid))))

(defun add-to-room (obj location)
  (update location)
  (pushnew (uuid obj) (contents location) :test #'equal)
  (setf (location obj) (uuid location))
  (save obj)
  (save location))

(defun get-location-by-id (id)
  (awhen (car (hashget (get-document *db* "_design/locations/_view/by_id" :key id)
                       "rows"))
    (make-instance 'location :document (get-document *db* (hashget it "key")))))

(defun ensure-location-design-doc ()
  (or (handler-case
          (get-document *db* "_design/locations")
        (document-not-found () nil))
      (put-document *db* "_design/locations"
                    (mkhash "language" "common-lisp"
                            "views" (mkhash "by_id"
                                            (mkhash "map"
                                                    (prin1-to-string
                                                     '(lambda (doc &aux (type (hashget doc "type")))
                                                       (when (equal type "location")
                                                         (emit (hashget doc "_id")
                                                               (hashget doc "description")))))))))))

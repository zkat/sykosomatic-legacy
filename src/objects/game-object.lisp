;; Copyright 2008 Kat Marchan

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

;; game-object.lisp
;;
;; Contains the base game-object class, and some relevant low-level functions
;; for dealing with objects.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Base Game Object
;;;
(defclass <game-object> ()
  ((name
    :initarg :name
    :initform ""
    :accessor name
    :documentation "Base name for the object")
   (desc
    :initarg :desc
    :initform ""
    :accessor desc
    :documentation "A description of the object")
   (desc-long
    :initarg :desc-long
    :initform ""
    :accessor desc-long
    :documentation "Long, detailed description of the object")
   (tags ;;not used in anything yet, but nifty idea
    :initarg :tags
    :initform nil
    :accessor tags
    :documentation "A list of string tags that target this object. First string is the name")
   (prox
    :initarg :prox
    :accessor prox
    :documentation "Object that this object is close to")
   (features
    :initarg :features
    :initform nil
    :accessor features
    :documentation "A list of OBJECTS that add more little details, all targetable :3"))
  (:documentation "Master game object. Contains base capabilities of all other objects in the game."))

;;;
;;; Load/Save
;;;
;;; Note: I think save-objects and load-objects could eventually be moved to a new db.lisp. The
;;;       file should do something else as well, though, so these will reside here, for now.

;;; Save

(defgeneric obj->file (obj path)
  (:documentation "Saves OBJECT to a file within PATH."))

(defun obj-list->files-in-dir (obj-list path)
  "Saves all OBJECTS in OBJECT-LIST into files within PATH"
  (loop for obj in obj-list
       do (obj->file obj path)))

(defun save-objects ()
  (save-players)
  (save-rooms)
  (format t "I think everything got saved. Hopefully, it did..."))

;;; Load

;; NOTE: It's alright for now, but we should, at some point, assert that the objects being
;;       loaded are of the appropriate type. This can be a vulnerability.

(defun file->obj (filepath)
  "Takes the FILEPATH of a file, returns the OBJECT it represents."
  (cl-store:restore filepath))

(defun files-in-path->obj-list (path file-extension)
  "Takes -all- files in PATH and collects them into a LIST of OBJECTS."
  (let ((files (directory (merge-pathnames (format nil "*.~a" file-extension) path))))
    (loop for file in files
	 collect (cl-store:restore file))))

(defun load-objects ()
  (load-players)
  (load-rooms)
  (format t "Apparently, everything got loaded."))


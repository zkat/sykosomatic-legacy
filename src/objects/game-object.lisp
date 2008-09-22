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
    :initform "NoNameObject"
    :accessor name
    :documentation "Base name for the object")
   (aliases
    :initarg :aliases
    :initform nil
    :accessor aliases
    :documentation "A list of aliases that can be used in place of NAME")
   (adjectives
    :initarg :adjectives
    :initform nil
    :accessor adjectives
    :documentation "List of adjectives that apply to this object. Different from name and aliases.")
   (desc
    :initarg :desc
    :initform "An object without a description"
    :accessor desc
    :documentation "A description of the object")
   (prox
    :initarg :prox
    :accessor prox
    :documentation "Object that this object is close to")
   (features
    :initarg :features
    :initform nil
    :accessor features
    :documentation "A list of OBJECTS that add more little details, all targetable."))
  (:documentation "Master game object. Contains base capabilities of all other objects in the game."))


;;;
;;; Info
;;;
(defgeneric look-description (game-object)
  (:documentation "Function that generates the appropriate LOOK-level description string."))

(defgeneric examine-description (game-object)
  (:documentation "Function that generates the appropriate EXAMINE-level description string."))

;;;
;;; Load/Save
;;;

;;; Save

(defgeneric obj->file (obj path)
  (:documentation "Saves OBJECT to a file within PATH."))

(defun obj-list->files-in-dir (obj-list path)
  "Saves all OBJECTS in OBJECT-LIST into files within PATH"
  (loop for obj in obj-list
       do (obj->file obj path)))

;;; Load

;; NOTE: It's alright for now, but we should, at some point, assert that the objects being
;;       loaded are of the appropriate type. This can be a vulnerability.
(defun file->obj (filepath)
  "Takes the FILEPATH of a file, returns the OBJECT it represents."
  (cl-store:restore filepath))

;; NOTE: This loads files indiscriminately. The files are currently not deleted if the object
;;       is deleted in-engine. Database stuff is going to need some major work.
(defun files-in-path->obj-list (path file-extension)
  "Takes -all- files in PATH and collects them into a LIST of OBJECTS."
  (let ((files (directory (merge-pathnames (format nil "*.~a" file-extension) path))))
    (loop for file in files
	 collect (cl-store:restore file))))

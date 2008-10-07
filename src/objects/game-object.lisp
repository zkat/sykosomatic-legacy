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
(in-package :org.sykosomatic.core)

;;;
;;; Base Game Object
;;;
(define-persistent-class <game-object> ()
  ((name
    :update
    :initarg :name
    :initform "NoNameObject"
    :accessor name
    :index-type hash-index
    :index-initargs (:test #'equalp)
    :index-reader objects-with-name
    :index-values all-objects
    :documentation "Base name for the object")
   (aliases
    :update
    :initarg :aliases
    :initform nil
    :accessor aliases
    :documentation "A list of aliases that can be used in place of NAME")
   (adjectives
    :update
    :initarg :adjectives
    :initform nil
    :accessor adjectives
    :documentation "List of adjectives that apply to this object. Different from name and aliases.")
   (desc
    :update
    :initarg :desc
    :initform "An object without a description"
    :accessor desc
    :documentation "A description of the object.")
   (features
    :update
    :initarg :features
    :initform nil
    :accessor features
    :documentation "A list of OBJECTS that add more little details, all targetable."))
  (:documentation "Master game object. Contains base capabilities of all other objects in the game."))

;;;
;;; Info
;;;

(defgeneric short-description (game-object)
  (:documentation "Function that generates the appropriate LOOK-level description string."))

;; TODO
(defmethod short-description ((object <game-object>))
  (desc object))

(defgeneric long-description (game-object)
  (:documentation "Function that generates the appropriate EXAMINE-level description string."))

(defmethod long-description ((object <game-object>))
  (desc object))

;;;
;;; Messages
;;;

(defgeneric write-to-target (target format-string &rest format-args)
  (:documentation "Formats a string and sends it to target game-object."))

(defmethod write-to-target ((target <game-object>) format-string &rest format-args)
  t)

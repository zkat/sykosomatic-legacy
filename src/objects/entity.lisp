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

;; entity.lisp
;;
;; Contains stuff related to the <entity> class, which is essentially one of two main forks of
;; <game-object> (the other being <room>). Stuff that inherits <entity> includes mobiles, items, etc
;; One special characteristic of entities is that they are meant to be contents of rooms.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Entity class
;;;
(define-persistent-class <entity> (<game-object>)
  ((name
    :update
    :initform "NoNameEntity"
    :index-type hash-index
    :index-reader entities-with-name
    :index-values all-entities)
   (location
    :update
    :initarg :location
    :initform nil
    :accessor location
    :documentation "Current location of the entity -- <room> object.")
   (invul-p
    :update
    :initarg invul-p
    :initform nil
    :accessor invul-p
    :documentation "Can entity take damage?")
   (hp
    :update
    :initarg :hp
    :initform 1 ;;everything should start with 1hp, if it's an entity. (= 0 DEATH)
    :accessor hp))
  (:documentation "'Stuff' like items, mobiles, etc. This is where objects fork away from rooms"))

;;;
;;; Info
;;;

(defun whereis (entity)
  "Pretty-prints the NAME of the LOCATION of the ENTITY"
  (let ((loc (location entity)) (entity (name entity)))
    (format t "~a is in ~a" entity (name loc))
    loc))

;;;
;;; Entity manipulation
;;;

(defgeneric put-entity (entity room)
  (:documentation "Changes where ENTITY is, taking care of any room-contents juggling."))

(defmethod put-entity ((entity <entity>) room) ; This should also make sure that the room where
  "Sets the LOCATION of ENTITY to ROOM."       ; <entity> currently resides has its CONTENTS
  (let ((old-room (location entity))
	(new-room room))
    (setf (location entity) new-room)
    (pushnew entity (contents new-room))
    (setf (contents old-room) (remove entity (contents old-room)))))


;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

;; sykosomatic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sykosomatic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sykosomatic.  If not, see <http://www.gnu.org/licenses/>.

;; game-object.lisp
;;
;; Contains the base game-object class, and some relevant low-level functions
;; for dealing with objects.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Entity class
;;;
(defclass <entity> (<game-object>)
  ((location
    :initarg :location
    :initform nil
    :accessor location
    :documentation "Current location of the entity -- <room> object.")
   (invul-p
    :initarg invul-p
    :initform nil
    :accessor invul-p
    :documentation "Can entity take damage?")
   (hp
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
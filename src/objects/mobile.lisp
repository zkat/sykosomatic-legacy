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

;; mobile.lisp
;;
;; Stuff related to mobiles, and everything that subclasses <mobile>
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Mobile class
;;;
(defclass <mobile> (<entity>)
  ((species
    :initarg :species
    :accessor species
    :documentation "Mobile's species")
   (killcount
    :initform 0
    :documentation "MURDER! DESTROY! BARSH!")
   (level
    :initarg :level
    :initform 1
    :accessor level
    :documentation "Power level (must be less than 9000)")
   (skills
    :initarg :skills
    :initform nil
    :accessor skills
    :documentation "A list of skills belonging to mobile")
   (inventory
    :initarg :inventory
    :initform nil
    :accessor inventory
    :documentation "A list of items in the player's possession"))
  (:documentation "Living things."))
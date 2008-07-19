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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;=====================================  Other classes  ========================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(in-package #:sykosomatic)

;;Master game object. Contains base capabilities of all other objects in the game.
(defclass <game-object> ()
  ((name
    :initarg :name
    :initform nil
    :accessor name
    :documentation "Base name for the object")
   (desc
    :initarg :desc
    :accessor desc
    :documentation "A description of the object")
   (desc-long
    :initarg :desc-long
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
    :documentation "Object that this object is approximate to")
   (features
    :initarg :features
    :accessor features
    :documentation "A list of OBJECTS that add more little details, all targetable :3")))

;; 'stuff'
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
    :accessor hp)))

(defclass <item> (<entity>)
  ((equippable
    :initarg :equip-p
    :initform nil
    :accessor equip-p
    :documentation "Can item be equipped?")
   (moveable
    :initarg :moveable
    :initform t
    :accessor moveable-p
    :documentation "Is this object movable? If nil, player cannot pick up")
   (effects
    :initarg :effects
    :accessor effects
    :documentation "Any special effects of the item")))

;; Living things
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
    :documentation "A list of items in the player's possession")))
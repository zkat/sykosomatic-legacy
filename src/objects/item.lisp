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

;; item.lisp
;;
;; Stuff related to items.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Item class
;;;
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
    :documentation "Any special effects of the item"))
  (:documentation "Master class for items."))
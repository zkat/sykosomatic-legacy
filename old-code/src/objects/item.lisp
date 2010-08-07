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

;; item.lisp
;;
;; Stuff related to items.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic.object)

;;;
;;; Item class
;;;
(define-persistent-class <item> (<entity>)
  ((name
    :update
    :initform "NoNameItem"
    :index-type hash-index
    :index-initargs (:test #'equalp)
    :index-reader items-with-name
    :index-values all-items)
   (equippable
    :update
    :initarg :equip-p
    :initform nil
    :accessor equip-p
    :documentation "Can item be equipped?")
   (moveable
    :update
    :initarg :moveable
    :initform t
    :accessor moveable-p
    :documentation "Is this object movable? If nil, avatar cannot pick up")
   (effects
    :update
    :initarg :effects
    :accessor effects
    :documentation "Any special effects of the item"))
  (:documentation "Master class for items. Mainly defines some basic things
that you can do with items, such as whether it's wearable, movable, or whether
it has any special effects."))

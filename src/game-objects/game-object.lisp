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

;; game-object.lisp
;;
;; Basic game objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defclass game-object ()
  ((name        :accessor name        :initarg :name)
   (pronoun     :accessor pronoun     :initarg :pronoun)
   (plural      :accessor plural      :initarg :plural)
   (description :accessor description :initarg :description)
   (adjectives  :accessor adjectives  :initarg :adjectives)
   (features    :accessor features    :initarg :features))
  (:default-initargs
   :adjectives nil
    :features nil))


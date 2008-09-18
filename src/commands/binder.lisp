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

;; binder.lisp
;;
;; Takes care of binding the various parts of the AST to their corresponding objects.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)


;;;
;;; Binder
;;;
;;; - Handles binding of AST to game-objects.

(defun bind-verb (verb)
  "Checks if VERB is a VERB. Returns a FUNCTION."
  (gethash verb *verbs*))

(defun bind-noun-phrase (noun-phrase scope-list)
  "Binds a noun-phrase into a single object, based on SCOPE."
 )

;;;
;;; Scope
;;;
;;; - These methods return the appropriate scope-list, given an object.

(defgeneric list-visible-objects (anchor)
  (:documentation "Returns a list of visible objects within the scope of the ANCHOR"))

(defmethod list-visible-objects ((room <room>))
  (contents room))

(defmethod list-visible-objects ((entity <entity>))
  (contents (location entity)))

(defmethod list-visible-objects ((mobile <mobile>))
  (append (contents (location mobile))
	  (inventory mobile)))

(defgeneric belongings (object)
  (:documentation "Returns a list of objects that belong to OBJECT."))

(defmethod belongings ((object <game-object>))
  ())

;;;
;;; Sexy builder
;;;
;;; - Builds the final s-expressions to be run by the event system.


;;;
;;; Util
;;;

;; NIL
;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic-ext

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

;; binder.lisp
;;
;; Takes care of binding the various parts of the AST to their corresponding objects.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic-ext)

;;;
;;; Binder
;;;
;;; - Handles binding of AST to game-objects.
;; AST to bind:
;; AST = (verb (noun-clause) (adverbs) chat-string)
;; noun-clause = (preposition (noun-group) (noun-group))
;; noun-group = (0*(noun-phrase))
;; noun-phrase = (noun (adjectives) belongs-to)
;; belongs-to = noun-phrase
;;
(defun bind-verb (verb)
  "Checks if VERB is a VERB. Returns a FUNCTION."
  (gethash verb *verbs*))

(defun bind-noun-clause (noun-clause scope-list)
  "Binds the noun-clause of the AST, returns a list of actual objects that
player commands can interpret."
  )

(defun bind-noun-group (noun-group scope-list)
  "Binds a noun-group within PLAYER's scope."
  )

;; NOTE: Make this a method that specializes on different objects. The binder then uses scope
;;       based on that object to figure out exactly how to bind a descriptor-list. OOP. mmm.
;; -- huh?
(defun bind-noun-phrase (noun-phrase scope-list)
  "Binds a noun-phrase into a single object, based on SCOPE."
  )

(defun resolve-possessive ())

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

;;;
;;; Sexy builder
;;;
;;; - Builds the final s-expressions to be run by the event system.

(defun parse-tree->sexp (player tree)
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  t)

(defun string->sexp (player string)
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp player (parse-string string)))

;;;
;;; Util
;;;

;; NIL
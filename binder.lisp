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

(defun bind-rest-of-sentence (scope rest-of-sentence)
  "Binds the rest-of-sentence part of the AST, returns a list of actual objects that
player commands can then interpret, and execute based upon."
  ;; Example rest-of-sentence
  ;; (preposition (bound-noun-phrase1) (bound-noun-phrase))
  ;; (nil (bound-noun-phrase1) nil)
  ;; (preposition nil (bound-noun-phrase2))
  
  )

(defun bind-noun-phrase (scope noun-phrase)
  "Binds a noun-phrase within PLAYER's scope."
  ;; Example noun-phrases:
  ;; (preposition object object)
  ;; (nil object nil)
  ;; (preposition nil  object)
  
  )

;; NOTE: Make this a method that specializes on different objects. The binder then uses scope
;;       based on that object to figure out exactly how to bind a descriptor-list. OOP. mmm.
(defun bind-descriptor-list (scope descriptor-list)
  "Binds a descriptor-list, which includes the name of an object, adjectives, pronouns,
and possessives. Returns a single object (the object being referred to)."
  ;; Example descriptor-list using possessives 
  ;; ("hilt" "heavy" "sword's" "the")
  ;;
  
  )

(defun bind-noun (scope noun)
  "Binds a noun, within SCOPE."
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

;;;
;;; Sexy builder
;;;
;;; - Builds the final s-expressions to be run by the event system.

;; TODO
(defun parse-tree->sexp (player tree)
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  t)

(defun string->sexp (player string)
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp player (parse-string string)))

;;;
;;; Util
;;;

(defun %possessive-p (word)
  "Is WORD in possessive form?"
  (let ((second-to-last-letter (elt word(- (length word) 2)))
	(last-letter (elt word (- (length word) 1))))
    (or (and (equal second-to-last-letter #\')
	     (equal last-letter #\s))
	(and (equal second-to-last-letter #\s)
	     (equal last-letter #\')))))

(defun possessive-p (word)
  "Nabs the actual word out of a possessive."
  (when (%possessive-p word)
    (car (cl-ppcre:split "'|'s" word))))


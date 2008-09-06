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
;;; Sexy builder
;;;

;; AST goes in, sexp goes out. (it builds s-exps, so it's sexy)
;; -----------------------------------------------
;; Goal AST - (emote rest-of-sentence adverb chat-string) ;;this will be expanded further.
;; ----------Where rest-of-sentence is ((noun-phrase) &optional (noun-phrase))
;; ---------------Where noun-phrase is ((descriptors) &optional (descriptors))
;; ---------------------where descriptors is ("noun" &rest "adjectives, articles, etc")
;; -----------------------------------------------

(defun string->function (string)
  "Checks if STRING is a VERB. Returns a FUNCTION."
  (gethash string *verbs*))

(defun parse-tree->sexp (player tree)
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  ;; FIXME: This could handle errors better. It should build the proper sexp now, though.
  (if (listp tree) ;change this to a WHEN once the silly write-to-client is disposed of
      (let ((verb (string->function (car tree)))
	    (emote (car tree))
	    (rest-of-sentence (bind-rest-of-sentence (cadr tree)))
	    (adverb-list (third tree))
	    (chat-string (fourth tree)))
	(list verb player (list emote rest-of-sentence adverb-list chat-string)))
      ;; Note about the following: This isn't a nice way to handle 'bad verb' conditions.
      (write-to-client (client player) "~&~a~&" tree)))

(defun string->sexp (player string)
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp player (parse-string string)))

;;;
;;; Binder
;;;

(defgeneric list-visible-objects (anchor)
  (:documentation "Returns a list of visible objects within the scope of the ANCHOR"))

(defmethod list-visible-objects ((room <room>))
  (contents room))

(defmethod list-visible-objects ((entity <entity>))
  (contents (location entity)))

(defmethod list-visible-objects ((mobile <mobile>))
  (append (contents (location mobile))
	  (inventory mobile)))

(defun bind-rest-of-sentence (player rest-of-sentence &key (scope :player))
  "Binds the rest-of-sentence part of the AST, returns a list of actual objects that
player commands can then interpret, and execute based upon."
  ;; Example rest-of-sentence
  ;; (preposition (bound-noun-phrase1) (bound-noun-phrase))
  ;; (nil (bound-noun-phrase1) nil)
  ;; (preposition nil (bound-noun-phrase2))
  
  )

(defun bind-noun-phrase (player noun-phrase &key (scope :player))
  "Binds a noun-phrase within PLAYER's scope."
  ;; Example noun-phrases:
  ;; (preposition object object)
  ;; (nil object nil)
  ;; (preposition nil  object)
  
  )

;; NOTE: Make this a method that specializes on different objects. The binder then uses scope
;;       based on that object to figure out exactly how to bind a descriptor-list. OOP. mmm.
(defun bind-descriptor-list (player descriptor-list &key (scope :player))
  "Binds a descriptor-list, which includes the name of an object, adjectives, pronouns,
and possessives. Returns a single object (the object being referred to)."
  ;; Example descriptor-list using possessives 
  ;; ("hilt" "heavy" "sword's" "the")
  ;;
  
  )


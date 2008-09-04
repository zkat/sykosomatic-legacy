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

;; NOTE: Basic idea of how to bind:
;;      * The car of the noun-phrase is the name of the actual object
;;      * Standalone words following this are searched for in the TAGS slot of the object
;;        until a unique match is found (if any at all)
;;      * If the binder comes across a single "s", then the next item is the name of the object
;;        the car is a feature of.

(defun bind-rest-of-sentence (player rest-of-sentence)
  "Binds the rest-of-sentence part of the AST, returns a list of actual objects that
player commands can then interpret, and execute based upon."
  ;; Example rest-of-sentence
  ;; ((bound-noun-phrase) preposition (bound-noun-phrase))
  ;; ((bound-noun-phrase) nil nil)
  ;; (nil preposition (bound-noun-phrase))

  )

(defun bind-noun-phrase (player noun-phrase)
  "Binds a noun-phrase within PLAYER's scope."
  ;; Example noun-phrases:
  ;; (object preposition object)
  ;; (object nil nil)
  ;; (nil preposition object)

  )

(defun bind-descriptor-list (player descriptor-list)
  "Binds a descriptor-list, which includes the name of an object, adjectives, pronouns,
and possessives. Returns a single object (the object being referred to)."
  ;; Example descriptor-list using possessives 
  ;; ("glimmer" "s" "hair" "green" "s" "syko")
  ;;
  ;; NOTE: I could possibly replace any instance of "s" with "of", to make it clearer, and bind
  ;;       references that use "of" and "'s" in the same way.
  )
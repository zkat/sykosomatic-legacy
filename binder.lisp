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
  (cdr (assoc string *verbs* :test #'string-equal)))

(defun parse-tree->sexp (player tree)
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (if (listp tree)
      (let ((verb (string->function (car tree)))
	    (emote (car tree))
	    (rest-of-sentence (cadr tree))
	    (chat-string (fourth tree))
	    (adverb (third tree)))
	(list verb player (list emote rest-of-sentence adverb chat-string)))
      ;; if it's not a list, it's an error from the parser. Spit that out, instead.
      (write-to-client (client player) "~&~a~&" tree)))

(defun string->sexp (player string)
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp player (parse-string string)))

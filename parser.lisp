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

(in-package #:sykosomatic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;===========================================  Parser  =========================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~ Pre-processing ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun preprocess-string (string)
  "Get rid of trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline) string))

;; TODO (when everything else is done)
(defun grammaritisize-chat-string (chat-string)
  "Cleans up the chat string and removes stupidity"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Tokenizer ~~~~~~~~~~~~~~~~~::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Notes: *This tokenizer (and the <word> class) assumes that there is only one pos per existing word.
;
;; Part 1: Raw string -> list of strings
;; -------------------------------------
;
(defun split-command-string (command-string)
  "Splits each COMMAND in COMMAND-STRING and puts it in a list of words-strings."
  (cl-ppcre:all-matches-as-strings "[a-zA-Z0-9!@$%&']{1,}" command-string))

(defun split-off-chat-string (string)
  "Takes a raw STRING and returns a LIST with COMMAND-STRING and CHAT-STRING"
  (cl-ppcre:split "^+'| +'|\"" string :limit 2))

(defun string->token-list (string)
  (let* ((com+chat (split-off-chat-string string))
	 (commands (split-command-string (car com+chat)))
	 (merged-command-list (append commands (cdr com+chat))))
    merged-command-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~ Parser ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TODO
;; A basic parser
;; Grammar:
;; Command ::= verb | verb noun-phrase
;; noun-phrase ::= noun | article noun
;; article ::= "a" | "an" | "the"
;; verb ::= *verbs*
;; noun ::= *any object in the scope of player*
;; -----------------------------------------------
;; !!! TODO
(defun parse-input (input) ;; well... something's something.
  (let* ((commands (string->token-list input))
	 (command-stack commands)
	 (current-token (pop command-stack)))
    (cond ((verb-p current-token) (list current-token))
	  (t (format t "unknown command ~a" current-token)))))

(defun verb-p (token)
  "Checks if a TOKEN is a VERB."
  (find token *verbs* :test #'string-equal))

(defun article-p (token)
  "Checks if TOKEN is an ARTICLE."
  (find token *articles* :test #'string-equal))

;; TODO
(defun noun-p (token)
  "Checks if TOKEN is a NOUN within scope."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~~ Binder ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; !!! I don't know how well the binder will deal with all the changes yet.
;; TODO
(defun noun->obj (noun) ;; This needs some info on scope to know what to bind to.
  "Takes a NOUN object and returns the OBJECT it refers to."
  (if (find (word noun) *objects* :test #'string-equal)
      (word noun)))

;; TODO
(defun verb->function (verb) ;;NOTE: Only accepts directions right now
  "Takes a VERB object and returns the FUNCTION the verb is supposed to call"
  (if (find (word verb) *directions* :test #'string-equal)
      (list #'move *current-player* (word verb))
      nil))

;; TODO
(defun parse-tree->sexp (tree) ;; This is really basic!
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (let ((verb (contents (left-child tree))) (noun (contents (right-child tree))))
    (cond ((and (verb-p verb) (not (noun-p noun))) (verb->function verb))
	  ((and (verb-p verb) (noun-p noun)) (append (verb->function verb) (noun->obj noun)))
	  (t nil))))

;;TODO
(defun string->sexp (string) ;; uses the basic abstract-syntax tree!!
  "Takes a STRING and turns it into a valid S-EXP FUNCTION to run."
  (parse-tree->sexp
   (obj-list->basic-ast
    (string->obj-list string))))
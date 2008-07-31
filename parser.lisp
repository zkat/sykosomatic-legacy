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
;String goes in, string-list goes out.
;; Part 1: Raw string -> list of strings
;; -------------------------------------
;
(defun split-command-string (command-string)
  "Splits each COMMAND in COMMAND-STRING and puts it in a list of words-strings."
  (cl-ppcre:all-matches-as-strings "[a-zA-Z0-9!@$%&']{1,}" command-string))

(defun split-off-chat-string (string)
  "Takes a raw STRING and returns a LIST with COMMAND-STRING and CHAT-STRING"
  (cl-ppcre:split "^+'| +'|\"" string :limit 2))

(defun format-chat-string (chat-string)
  (if chat-string
      (format nil "'~a" chat-string)
      nil))

(defun concat-format-chat-string (chat-string)
  (if chat-string
      (concatenate 'string "'" chat-string)
      nil))

(defun chat-string->token (chat-string)
  "Takes a CHAT-STRING, returns the corresponding <TOKEN>"
  (make-instance '<token> :token-string chat-string :type :chat-string))

(defun string->token-list (string) ;;this is so ugly. Whatever...
  "Converts a STRING into a LIST of TOKEN-STRINGS."
  (let* ((com+chat (split-off-chat-string string))
	 (commands (split-command-string (car com+chat)))
	 (token-list (append commands (list (format-chat-string (cadr com+chat))))))
    token-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~ Parser ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This whole section: string-list goes in, AST comes out.
;; A basic parser
;; Grammar:
;; Command ::= verb [noun-phrase]
;; Noun-phrase ::= [article] noun
;; article ::= "a" | "an" | "the"
;; verb ::= *verbs*
;; noun ::= *any object in the scope of player*
;; -----------------------------------------------
;; !!! TODO - make the parser more complete.

(defun parse-player-input (player string)
  "Parses a STRING that was entered by PLAYER and returns a S-EXP"
  (parse-command player (string->token-list string)))

(defun parse-command (player token-list)
  (let ((verb (parse-verb (car token-list)))
	(noun-phrase (parse-noun-phrase player (cdr token-list))))
    (cond (verb
	   (if noun-phrase
	       (list verb player noun-phrase))
	       (list verb player))
	  (t 
	   (format t "~%Unknown verb: '~a'~%" (car token-list))))))

(defun parse-verb (string)
  "Checks if STRING is a VERB. Returns a FUNCTION."
  (cdr (assoc string *verbs* :test #'string-equal)))

(defun parse-article (string)
  "Checks if STRING is an ARTICLE. Returns a KEYWORD."
  (cdr (assoc string *articles* :test #'string-equal)))

(defun parse-noun (player string)
  "Checks if STRING is a NOUN within PLAYER'S scope. Returns an OBJECT."
  t)

(defun parse-noun-phrase (player token-list)
  "Parses a TOKEN-LIST. Returns a LIST depicting a NOUN-PHRASE. Uses PLAYER'S scope to find objects."
  (if (parse-article (car token-list))
      (list (parse-noun player (cadr token-list)) (parse-article (car token-list)))
      (list (parse-noun player (car token-list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~~ Binder ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; AST goes in, function goes out.
;This whole thing has to be written within the context of the main loop.
;; TODO
(defun noun->obj (noun) ;; This needs some info on scope to know what to bind to.
  "Takes a NOUN object and returns the OBJECT it refers to."
  (if (member (word noun) *objects* :test #'string-equal)
      (word noun)))

;; TODO
(defun verb->function (verb) ;;NOTE: Only accepts directions right now
  "Takes a VERB object and returns the FUNCTION the verb is supposed to call"
  (if (member (word verb) *directions* :test #'string-equal)
      (list #'move *current-player* (word verb))
      nil))

;; TODO
(defun parse-tree->sexp (tree) ;; This is really basic!
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (let ((verb (contents (left-child tree))) (noun (contents (right-child tree))))
    (cond ((and (verb-p verb) (not (noun-p noun))) 
	   (verb->function verb))
	  ((and (verb-p verb) (noun-p noun)) 
	   (append (verb->function verb) (noun->obj noun)))
	  (t
	   nil))))

;;TODO
(defun string->sexp (string) ;; uses the basic abstract-syntax tree!!
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp
   (obj-list->basic-ast
    (string->obj-list string))))
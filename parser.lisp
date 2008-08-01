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
(defun prompt-user ()
  "Prompts the user for input, and returns a string."
  (format t "~%~%-> ")
  (read-line))

(defun preprocess-string (string)
  "Get rid of trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline) string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Tokenizer ~~~~~~~~~~~~~~~~~::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;String goes in, string-list goes out.
;
(defun split-command-string (command-string)
  "Splits each COMMAND in COMMAND-STRING and puts it in a list of words-strings."
  (cl-ppcre:all-matches-as-strings "[a-zA-Z0-9!/@$%&']{1,}" command-string))

(defun split-off-chat-string (string)
  "Takes a raw STRING and returns a LIST with COMMAND-STRING and CHAT-STRING"
  (cl-ppcre:split "^+'| +'|\"" string :limit 2))

(defun format-chat-string (chat-string)
  "Adds a ['] to the beginning of the CHAT-STRING. Used to tell it apart from other parts of the sentence."
  (if chat-string
      (format nil "'~a" chat-string)
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
;; This whole section: string-list goes in, AST comes out.
;; A basic parser
;; Grammar:
;; Command ::= verb [noun-phrase]
;; Noun-phrase ::= [article] noun
;; article ::= "a" | "an" | "the"
;; verb ::= *verbs*
;; noun ::= *any object in the scope of player*
;; -----------------------------------------------
;; !!! TODO - add complexity to the parser.
;
(defun parse-string (string)
  "Parses a STRING that was entered by PLAYER and returns an Abstract Syntax Tree"
  (parse-command (string->token-list string)))

(defun parse-command (token-list)
  "Uses a TOKEN-LIST to generate an AST"
  (if (verb-p (car token-list))
      (let ((noun-phrase (parse-noun-phrase (cdr token-list)))
	    (verb (car token-list)))
	(if noun-phrase
	    (list verb noun-phrase)
	    (list verb)))
      (format t "Unknown verb: '~a'" (car token-list))))

;; These are the only ones I have to change! :D
(defun verb-p (string)
  "Is STRING a VERB?"
  (assoc string *verbs* :test #'string-equal))

(defun article-p (string)
  "Is STRING an ARTICLE?"
  (assoc string *articles* :test #'string-equal))

(defun parse-noun-phrase (token-list)
  "Parses a TOKEN-LIST into an LIST representing a NOUN PHRASE."
  (if (article-p (car token-list))
      (list (cadr token-list) (car token-list))
      (list (car token-list))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~~ Binder ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST goes in, function goes out.
;; !!! TODO - make sure any changes to the parser get mirrored here.
;
;; TODO
(defun noun->obj (player noun) ;; Only handles directions. Can be expanded relatively easily.
  "Takes a NOUN object and returns the OBJECT it refers to."
  (find noun *directions* :test #'string-equal))

(defun verb->function (string)
  "Checks if STRING is a VERB. Returns a FUNCTION."
  (cdr (assoc string *verbs* :test #'string-equal)))

;; TODO
(defun parse-tree->sexp (player tree) ;; maybe the best way to handle this is for the functions to take care of evaluating the noun.
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (let ((verb (verb->function (first tree)))
	(noun-phrase (cadr tree)))
    (list verb player noun-phrase)))

(defun string->sexp (player string)
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp player
		    (parse-string string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ EXECUTOR!! ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun execute-command (player string)
  "Takes a STRING and EXECUTES the appropriate command within PLAYER's context."
  (let ((sexp (string->sexp player string)))
    (if (functionp (car sexp))
	(apply (car sexp) (cdr sexp)))))
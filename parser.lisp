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

(defun string-list->token-list (string-list)
  "Turns a list of STRINGS into a list of <TOKENS>"
  (apply #'string->token string-list))

(defun chat-string->token (chat-string)
  "Takes a CHAT-STRING, returns the corresponding <TOKEN>"
  (make-instance '<token> :token-string chat-string :type :chat-string))

;; NOTE: I'm creating one of the the actual <token> instances here, a little early.
;; This is an easy way to tokenize the chat-string. I don't know how to do it later than this step.
(defun string->token-list (string) 
  "Converts a STRING into a LIST of TOKENS."
  (let* ((com+chat (split-off-chat-string string))
	 (commands (split-command-string (car com+chat)))
	 (merged-command-list (append commands (list (chat-string->token (cdr com+chat))))))
    merged-command-list))

(defclass <token> () ;; this only involves modifying string->token-list and the predicates in parser
  ((token-string
    :initarg :token-string
    :initform (error "Must provide a :word-string")
    :accessor token-string)
   (type
    :initarg :type
    :initform (error "Must supply a :type")
    :accessor type)))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~ Parser ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; A basic parser
;; Grammar:
;; Command ::= verb [noun-phrase]
;; noun-phrase ::= [article] noun
;; article ::= "a" | "an" | "the"
;; verb ::= *verbs*
;; noun ::= *any object in the scope of player*
;; -----------------------------------------------
;; !!! TODO - make the parser more complete.

(defun parse-command (token-list)
  "Parses a TOKEN-LIST into an ABSTRACT SYNTAX TREE."
  (let ((verb (verb-p (car token-list)))
	(noun-phrase (parse-noun-phrase (cdr token-list))))
  (if verb
      (if noun-phrase
	  (list verb noun-phrase)
	  (list verb))
      (format t "~%Unknown verb: '~a'~%" (car token-list)))))
  
(defun parse-noun-phrase (token-list)
  "Parses a TOKEN-LIST into a NOUN-PHRASE list."
  (if (article-p (car token-list))
   (list (second token-list) (car token-list))
   (list (car token-list))))

(defun verb-p (token)
  "Checks if a TOKEN is a VERB."
  (find token *verbs* :test #'string-equal))

(defun article-p (token)
  "Checks if TOKEN is an ARTICLE."
  (find token *articles* :test #'string-equal))

(defun old-verb-p (token)
  "Checks if a TOKEN is a VERB."
  (find token *verbs* :test #'string-equal))

(defun old-article-p (token)
  "Checks if TOKEN is an ARTICLE."
  (find token *articles* :test #'string-equal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~~ Binder ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; !!! Has to be rewritten to take in the new AST
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
    (cond ((and (verb-p verb) (not (noun-p noun))) (verb->function verb))
	  ((and (verb-p verb) (noun-p noun)) (append (verb->function verb) (noun->obj noun)))
	  (t nil))))

;;TODO
(defun string->sexp (string) ;; uses the basic abstract-syntax tree!!
  "Takes a STRING and turns it into a valid S-EXP FUNCTION to run."
  (parse-tree->sexp
   (obj-list->basic-ast
    (string->obj-list string))))
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
  (all-matches-as-strings "[a-zA-Z0-9!/@$%&']{1,}" command-string))

(defun split-off-chat-string (string)
  "Takes a raw STRING and returns a LIST with COMMAND-STRING and CHAT-STRING"
  (split "^+'| +'|\"" string :limit 2))

(defun format-chat-string (chat-string)
  "Adds a ['] to the beginning of the CHAT-STRING. Used to tell it apart from other parts of the sentence."
  (if chat-string
      (format nil "'~a" chat-string)
      nil))

(defun string->token-list (string)
  "Converts a STRING into a LIST of TOKEN-STRINGS."
  (let* ((com+chat (split-off-chat-string string))
	 (commands (split-command-string (car com+chat))))
    (if (cadr com+chat)
	(let ((chat-string (format-chat-string (cadr com+chat))))
	  (append commands (list chat-string)))
	commands)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~ Parser ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This whole section: string-list goes in, AST comes out.
;; -----------------------------------------------
;; The complete parser
;; Command ::= (adverb) verb (adverb) ((pronoun)<noun-phrase> (adverb) (preposition <noun-phrase> (adverb)))
;; Noun-phrase ::= <noun-group> (preposition <noun-group>)
;; noun-group ::= (pronoun) | ((article) (number) (adjective) string) ;;whoever wrote this doesn't know english :-\
;; ----------------------------------------------
;; The goal AST:
;; ("verb" (rest-of-predicate) adverbs "chat-string"))
;; where (rest-of-predicate) is (noun-phrase-1 preposition noun-phrase-2)
;; where (noun-phrase) is (noun (modifiers))
;;
;
(defun parse-string (string)
  "Parses a STRING that was entered by PLAYER and returns an Abstract Syntax Tree"
  (parse-command (string->token-list string)))

(defun preparse-adverbs (token-list) ;The Final Solution to the Adverb problem.
  "Yoinks all the adverbs it recognizes out of a list.
MULTIPLE RETURN VALUES: The first adv it finds, and a token-list purified of this evil."
  (if (> (length token-list) 1)
      (let ((adverb (find-if #'adverb-p token-list)))
	(if adverb
	    (let ((token-list (remove-if #'adverb-p token-list :count 1)))
	      (values adverb token-list))
	    (values adverb token-list)))
      (values nil token-list)))

(defun parse-command (token-list)
  "Uses a TOKEN-LIST to generate an AST"
  (multiple-value-bind (adverb token-list) (preparse-adverbs token-list)
    (cond ((chat-string-p (car token-list))
	   (list "say" nil adverb (car token-list)))
	  ((verb-p (car token-list))
	   (let ((verb (car token-list))
		 (token-list (cdr token-list)))
	     (multiple-value-bind (noun-phrase token-list) (parse-noun-phrase token-list)
	       (if (chat-string-p (car token-list))
		   (let ((chat-string (car token-list)))
		     (list verb noun-phrase adverb chat-string))
		   (list verb noun-phrase adverb nil)))))
	  (t
	   (format t "Unknown verb: '~a'" (car token-list))))))

(defun parse-noun-phrase (token-list) 
  "Parses a TOKEN-LIST into an LIST representing a NOUN PHRASE.
MULTIPLE RETURN VALUES: NOUN-PHRASE and REST OF THE TOKEN LIST."
  (multiple-value-bind (noun-group-1 token-list) (parse-noun-group token-list)
    (if (preposition-p (car token-list))
	(let ((preposition (car token-list)))
	  (multiple-value-bind (noun-group-2 token-list) (parse-noun-group (cdr token-list))
	    (values (list noun-group-1 preposition noun-group-2) token-list)))
	(values (list noun-group-1) token-list))))
  
(defun parse-noun-group (token-list)
  "Parses a TOKEN-LIST into a LIST representing a NOUN GROUP.
MULTIPLE RETURN VALUES: NOUN-GROUP and REST of the TOKEN-LIST."
  (cond ((or (preposition-p (car token-list))
	     (null (car token-list))
	     (chat-string-p (car token-list)))
	 (values nil token-list))
	((or (preposition-p (cadr token-list))
	     (chat-string-p (cadr token-list)))
	 (values (list (car token-list)) (cdr token-list)))
	(t
	 (let ((descriptor (car token-list)))
	   (multiple-value-bind (descriptors token-list) (parse-noun-group (cdr token-list))
	     (values (append descriptors (list descriptor))
		     token-list))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~ Predicates ~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun verb-p (string)
  "Is STRING a VERB?"
  (assoc string *verbs* :test #'string-equal))

(defun chat-string-p (string)
  "Is STRING a CHAT-STRING?"
  (if (not (null string))
      (char-equal #\' (char string 0))))
 
(defun preposition-p (string)
  "Is STRING a PREPOSITION?"
  (member string *prepositions* :test #'string-equal))
 
(defun adverb-p (string)
  "Is STRING an ADVERB?"
  (member string *adverbs* :test #'string-equal))

;; Util
(defun test-the-parser ()
  "Runs a loop that asks for player input and returns whatever gets parsed. Quits on 'quit'."
  (let ((current-input (prompt-user)))
    (if (string-equal current-input "quit")
	(format t "Bye bye!")
	(progn 
	  (let ((parse-tree (parse-string current-input)))
	    (format t "~a" parse-tree))
	  (test-the-parser)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Sexy Builder ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST goes in, sexp goes out. (it builds s-exps, so it's sexy)
;; -----------------------------------------------
;; Goal AST - (#'verb emote rest-of-sentence adverb chat-string) ;;this will be expanded further.
;; ----------Where rest-of-sentence is ((noun-phrase) &optional (noun-phrase))
;; ---------------Where noun-phrase is ((descriptors) &optional (descriptors))
;; ---------------------where descriptors is ("noun" &rest "adjectives, articles, etc")
;; -----------------------------------------------
;; !!! TODO - make sure any changes to the parser get mirrored here.

(defun verb->function (string)
  "Checks if STRING is a VERB. Returns a FUNCTION."
  (cdr (assoc string *verbs* :test #'string-equal)))

(defun parse-tree->sexp (tree)
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (let ((verb (verb->function (car tree)))
	(emote (car tree))
	(rest-of-sentence (cadr tree))
	(chat-string (fourth tree))
	(adverb (third tree)))
    (list verb emote rest-of-sentence adverb chat-string)))

(defun string->sexp (string)
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp (parse-string string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ EXECUTOR!! ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun execute-command (player string)
  "Takes a STRING and EXECUTES the appropriate command within PLAYER's context."
  (let ((sexp (string->sexp string)))
    (if (functionp (car sexp))
	(apply (car sexp) (cdr sexp)))))

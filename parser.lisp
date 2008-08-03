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
;; -----------------------------------------------
;; The complete parser
;; Command ::= (adverb) verb (adverb) ((pronoun)<noun-phrase> (adverb) (preposition <noun-phrase> (adverb)))
;; Noun-phrase ::= <noun-group> (preposition <noun-group>)
;; noun-group ::= (pronoun) | ((article) (number) (adjective) string) ;;whoever wrote this doesn't know english :-\
;; verb ::= *verbs*
;; adverb ::= *adverbs*
;; preposition ::= *prepositions*
;; article ::= *articles*
;; pronoun ::= *pronouns*
;; number ::= number, or number-string
;; adjective ::= any unknown token, can be used later.
;; noun ::= any unknown token, can be used later.
;; ----------------------------------------------
;; The goal AST:
;; ("verb" (adverbs) (rest-of-predicate) "chat-string"))
;; where (rest-of-predicate) is (noun-phrase-1 preposition noun-phrase-2)
;; where (noun-phrase) is (noun (modifiers))
;;
;; !!! TODO - Add everything to the parser.
;
;;; Ignoring adverbs for now. It's worth noting that I should be able to add them as soon as I know
;;; how to parse noun-groups properly and completely.

(defun parse-string (string)
  "Parses a STRING that was entered by PLAYER and returns an Abstract Syntax Tree"
  (parse-command (string->token-list string)))

(defun parse-command (token-list)
  "Uses a TOKEN-LIST to generate an AST"
  (let ((adverbs nil))
    (cond ((verb-p (car token-list))
	   (let ((verb (car token-list)))
	     (if (adverb-p (cadr token-list))
		 (let ((adverbs (append (list (cadr token-list)) adverbs)))
		   (multiple-value-bind (noun-phrase token-list) (parse-noun-phrase 
								    (cddr token-list))
		     (if (car token-list)
			 (format t "Don't know what ~a is." (car token-list))
			 (list verb adverbs noun-phrase nil))))
		 (multiple-value-bind (noun-phrase token-list) (parse-noun-phrase (cdr token-list))
		   (if (chat-string-p (car token-list))
		       (let ((chat-string (car token-list)))
			 (list verb adverbs noun-phrase chat-string))
		       (list verb adverbs noun-phrase nil))))))
	  (t
	    (format t "Unknown verb: '~a'" (car token-list))))))
	   
(defun parse-noun-phrase (token-list)
  "Parses a TOKEN-LIST into an LIST representing a NOUN PHRASE. 
MULTIPLE RETURN VALUES: NOUN-PHRASE, and REST OF THE TOKEN LIST."
  (multiple-value-bind (noun-group-1 token-list) (parse-noun-group token-list)
    (if (preposition-p (car token-list))
	(let ((preposition (car token-list)))
	  (multiple-value-bind (noun-group-2 token-list) (parse-noun-group (cdr token-list))
	    (values 
	     (list noun-group-1 preposition noun-group-2)
	     token-list)))
	(values (list noun-group-1) token-list))))

(defun parse-noun-group (token-list)
  "Parses a TOKEN-LIST into a NOUN-GROUP."
  (if (pronoun-p (car token-list))
      (values (list (car token-list)) (cdr token-list)) ;if it's a pronoun, we're all set. :)
      (multiple-value-bind (adj-phrase token-list) 
	  (reverse (parse-adjectival-phrase token-list))
	(values adj-phrase token-list))))

(defun parse-adjectival-phrase (token-list) ;;OH GOD WHY D:
  "Parses a TOKEN-LIST into an ADJECTIVAL PHRASE."
  (if (not (or (adverb-p (car token-list))
	       (preposition-p (car token-list))
	       (null (car token-list))))
      (values (append (list (car token-list)) (parse-adjectival-phrase (cdr token-list))) (cdddr token-list)) 
      nil))

;; (defun parse-noun-group (token-list) ;;THIS IS THE MOST AWFUL CRAP EVER.
;;   "Parses a NOUN-GROUP from a TOKEN LIST.
;; Returns a NOUN-GROUP list, and the rest of the token list it didn't parse."
;;   (cond ((pronoun-p (car token-list))
;; 	 (values (list (car token-list)) (cdr token-list)))
;; 	((article-p (car token-list))
;; 	 (let ((article (car token-list))
;; 	       (token-list (cdr token-list))))
;; 	   (cond ((number-qualifier-p (car token-list))
;; 		  (let ((number-qualifier (car token-list))
;; 			(token-list (cdr token-list)))
;; 		    (if (and (not (adverb-p (car token-list)))
;; 			     (not (preposition-p (cadr token-list))))
;; 			(if ((and (not (adverb-p (cadr token-list)))
;; 				  (not (preposition-p (car token-list))))
;; 			     (let ((adjective (car token-list))
;; 				   (noun (cadr token-list))
;; 				   (token-list (cddr token-list)))
;; 			       (values (list noun article number-qualifier adjective) token-list))
;; 			     (let ((noun (car token-list))
;; 				   (token-list (cdr token-list)))
;; 			       (values (list noun article number-qualifier) token-list))))
;; 			(progn (format t "~a is not a noun or adjective.")
;; 			       (values nil nil)))))
;; 		 ((
				  
(defun chat-string-p (string)
  "Is STRING a CHAT-STRING?."
  (if string 
      (char-equal #\' (char string 0))))

(defun possessive-p (string)
  "Is STRING a POSSESSIVE?"
  (find #\' string :test #'char-equal))
   
(defun verb-p (string)
  "Is STRING a VERB?"
  (assoc string *verbs* :test #'string-equal))

(defun preposition-p (string)
  "Is STRING a PREPOSITION?"
  (member string *prepositions* :test #'string-equal))

(defun adjective-p (string)
  "Is STRING a PREPOSITION?"
  (member string *adjectives* :test #'string-equal))

(defun article-p (string)
  "Is STRING an ARTICLE?"
  (member string *articles* :test #'string-equal))

(defun adverb-p (string)
  "Is STRING an ADVERB?"
  (member string *adverbs* :test #'string-equal))

(defun pronoun-p (string)
  "Is STRING a PRONOUN?"
  (member string *pronouns* :test #'string-equal))
;;need a numberp of some sort here

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~~ Binder ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; AST goes in, function goes out.
;; Goal AST - (#'verb player noun-phrase emote) ;;this will be expanded further.
;; -----------------------------------------------
;; Expanded AST - (#' player (noun-phrases) (adverbs) emote) 
;; where noun-phrases is (noun-group preposition noun-group)
;; where noun-group is (noun (identifiers))
;; -----------------------------------------------
;; !!! TODO - make sure any changes to the parser get mirrored here.

(defun verb->function (string)
  "Checks if STRING is a VERB. Returns a FUNCTION."
  (cdr (assoc string *verbs* :test #'string-equal)))

;; TODO
(defun parse-tree->sexp (player tree) ;; Do I even need the player for this part? No, but I have to change a lot.
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (let ((verb (verb->function (car tree)))
	(emote (car tree))
	(noun-phrase (cadr tree)))
    (list verb player noun-phrase emote)))

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
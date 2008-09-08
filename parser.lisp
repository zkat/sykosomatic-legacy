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

;; parser.lisp
;;
;; Cleans up and parses a string, generating an abstract syntax tree. Needs access to
;; a list of verbs and adverbs in order to generate the tree accurately.
;; Also has some functions for saving/loading some vocabulary.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Pre-processing
;;;
;;; - Cleans up the incoming string
(defun prompt-user ()
  "Prompts the user for input, and returns a string."
  (format t "~%~%-> ")
  (read-line))

(defun preprocess-string (string)
  "Get rid of trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline #\Return) string))

;;;
;;; Tokenizer
;;;
;;; - String goes in, string-list goes out.

(defun split-command-string (command-string)
  "Splits each COMMAND in COMMAND-STRING and puts it in a list of words-strings."
  (cl-ppcre:all-matches-as-strings "[a-zA-Z0-9@/#$^&*']{1,}" command-string))

(defun split-off-chat-string (string)
  "Takes a raw STRING and returns a LIST with COMMAND-STRING and CHAT-STRING"
  (cl-ppcre:split " +'|\"" string :limit 2))

(defun format-chat-string (chat-string)
  "Adds a ['] to the beginning of the CHAT-STRING. Used to tell it apart from other parts of the sentence."
  (if chat-string
      (format nil "'~a" chat-string)
      nil))

(defun string->token-list (string)
  "Converts a STRING into a LIST of TOKEN-STRINGS."
  (let* ((com+chat (split-off-chat-string (preprocess-string string)))
	 (commands (split-command-string (car com+chat))))
    (if (cadr com+chat)
	(let ((chat-string (format-chat-string (cadr com+chat))))
	  (append commands (list chat-string)))
	commands)))

;;;
;;; Parser
;;;
;;; - Takes a string-list, and returns an AST.

;; string-list goes in, AST comes out.
;; -----------------------------------------------
;; The complete parser
;; TODO: expand noun-group to account for multiple nouns and possessives.
;; Command = 1*[adverb] verb [adverb] [[pronoun] noun-phrase [adverb] [preposition <noun-phrase> [adverb]]]
;; Noun-phrase ::= noun-group [preposition noun-group]
;; noun-group ::= ([pronoun] / [[article] [number] [adjective] string] 
;; ----------------------------------------------
;; Goal AST - (emote rest-of-sentence adverb chat-string) ;;this will be expanded further.
;; -----------Where REST-OF-SENTENCE is ((noun-phrase) &optional (noun-phrase))
;; -----------Where NOUN-PHRASE is ((descriptors) &optional (descriptors))
;; -----------Where DESCRIPTORS is ("noun" &rest "adjectives, articles, etc")
;;

(defun parse-string (string)
  "Parses a STRING that was entered by PLAYER and returns an Abstract Syntax Tree"
  (parse-sentence (string->token-list string)))

;; TODO: Implement proper adverb parsing
(defun preparse-adverbs (token-list) 
  "Yoinks all the adverbs it recognizes out of a list.
MULTIPLE RETURN VALUES: The first adv it finds, and a token-list purified of this evil."
  ;; This is the worst way to handle this shit. Ever. Temporary. Very.
  (if (> (length token-list) 1)
      (let ((adverb (find-if #'adverb-p token-list)))
	(if adverb
	    (let ((token-list (remove adverb token-list :test #'string-equal :count 1)))
	      (values (list adverb) token-list))
	    (values (list adverb) token-list)))
      (values nil token-list)))

(defun parse-sentence (token-list)
  "Uses a TOKEN-LIST to generate an AST"
  (multiple-value-bind (adverbs token-list) (preparse-adverbs token-list)
    (cond ((chat-string-p (car token-list)) 
	   (list "say" nil adverbs (car token-list)))
	  ((verb-p (car token-list))
	   (let ((verb (car token-list))
		 (token-list (cdr token-list)))
	     (multiple-value-bind (rest-of-predicate token-list) (parse-rest-of-predicate token-list)
	       (cond ((null token-list)
		      (list verb rest-of-predicate adverbs nil))
		     ((chat-string-p (car token-list))
		      (let ((chat-string (car token-list)))
			(list verb rest-of-predicate adverbs chat-string)))
		     (t
		      (list verb rest-of-predicate adverbs nil))))))
	  (t
	   (let ((fail-verb (car token-list)))
	     (if fail-verb
		 (error 'unknown-verb-error :text "Unknown verb" :verb fail-verb)
		 (error 'parser-error :text "Invalid input")))))))

(defun parse-rest-of-predicate (token-list)
  "Generates the REST-OF-PREDICATE list.
MULTIPLE RETURN VALUES: REST-OF-PREDICATE list, and the remaining TOKEN-LIST"
  (multiple-value-bind (noun-phrase-1 token-list) (parse-noun-phrase token-list)
    (if (preposition-p (car token-list))
	(let ((preposition (car token-list))
	      (token-list (cdr token-list)))
	  (multiple-value-bind (noun-phrase-2 token-list) (parse-noun-phrase token-list)
	    (values (list preposition noun-phrase-1 noun-phrase-2) token-list)))
	(values (list nil noun-phrase-1 nil) token-list))))

(defun parse-noun-phrase (token-list)
  "Parses a TOKEN-LIST into an LIST representing a NOUN PHRASE.
MULTIPLE RETURN VALUES: NOUN-PHRASE and REST OF THE TOKEN LIST."
  (multiple-value-bind (noun-group-1 token-list) (parse-noun-group token-list)
    (if (preposition-p (car token-list))
	(let ((preposition (car token-list)))
	  (multiple-value-bind (noun-group-2 token-list) (parse-noun-group (cdr token-list))
	    (values (list preposition noun-group-1  noun-group-2) token-list)))
	(values (list nil noun-group-1 nil) token-list))))

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

;;;
;;; Predicates
;;;

(defun verb-p (string)
  "Is STRING a VERB?"
  (gethash string *verbs*))

(defun chat-string-p (string)
  "Is STRING a CHAT-STRING?"
  (unless (null string)
    (char-equal #\' (char string 0))))

(defun preposition-p (string)
  "Is STRING a PREPOSITION?"
  (gethash string *prepositions*))

(defun adverb-p (string)
  "Is STRING an ADVERB?"
  (gethash string *adverbs*))

;;;     
;;; Util
;;;

(define-condition unknown-verb-error (error)
  ((text :initarg :text :reader text)
   (verb :initarg :verb :reader verb))
  (:documentation "Signaled whenever an unknown verb is encountered.")
  (:report (lambda (condition stream)
	     (format stream "Tried to parse an unknown verb: ~A" (verb condition)))))

(define-condition parser-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Condition signaled whenever some generic parsing error happens.")
  (:report "Parser probably got some garbage input, or an empty string."))

(defun test-the-parser ()
  "Runs a loop that asks for player input and returns whatever gets parsed. Quits on 'quit'."
  (let ((current-input (prompt-user)))
    (if (string-equal current-input "quit")
	(format t "Bye bye!")
	(progn
	  (let ((parse-tree (parse-string current-input)))
	    (format t "~%AST Generated: ~A~%" parse-tree))
	  (test-the-parser)))))


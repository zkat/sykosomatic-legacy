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
;; Cleans up and parses a string, generating an abstract syntax tree.
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
  (cl-ppcre:all-matches-as-strings "[a-zA-Z0-9@/#$^&*']{1,}|," command-string))

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

;; ABNF grammar - http://en.wikipedia.org/wiki/ABNF
;; ------------
;;
;; sentence =  chat-string
;; sentence =/ [adverb] verb [noun-clause] [adverb] [chat-string]
;;
;; noun-clause =  noun-phrase
;; noun-clause =/ [noun-phrase] [[adverb] [preposition] noun-phrase]
;;
;; noun-phrase =  pronoun
;; noun-phrase =/ [article] [numeral] [adjective] \
;;               (noun / noun conjunction noun-phrase / possessive-noun phrase)
;;
;; article = satisfies article-p
;; numeral = satisfies numeral-p
;; adjective = any unknown token that comes before a noun or a possessive
;; noun = anything before a preposition or conjunction
;; pronoun = satisfies pronoun-p
;; possessive-noun = satisfies possessive-p (['s] or [s'])
;; conjunction = satisfies conjunction-p (i.e. "and" "&" "," etc.)
;;
;; ----------------------------------------------
;; TODO: What does the AST for the new noun-group grammar look like?
;;
;; Goal AST - (verb noun-clause adverb-list chat-string) ;;this will be expanded further.
;; -----------Where NOUN-CLAUSE is (preposition noun-phrase noun-phrase)
;; -----------Where NOUN-PHRASE is (list-of-objects)  update
;;;; NOTE: I can grab a list of possessives and use (reduce #'list possessive-list) on them, in the
;;;;       order that I want the recursion to go in!!

(defun parse-string (string)
  "Parses a STRING that was entered by PLAYER and returns an Abstract Syntax Tree"
  (parse-sentence (string->token-list string)))

;; TODO: ALRIGHT. LET'S REWRITE THIS BITCH.

(defun parse-sentence (token-list)
  "Uses a TOKEN-LIST to generate an AST"
  (let ((verb nil)
	(noun-clause nil)
	(adverb-1 nil)
	(adverb-2 nil)
	(adverb-3 nil)
	(chat-string nil))
    (when (adverb-p (car token-list))
      (setf adverb-1 (pop token-list)))
    (cond ((chat-string-p (car token-list))
	   (setf verb "say")
	   (setf chat-string (pop token-list)))
	  ((verb-p (car token-list))
	   (setf verb (pop token-list))
	   (multiple-value-setq (noun-clause adverb-2 token-list) (parse-noun-clause token-list))
	   (when (adverb-p (car token-list))
	     (setf adverb-3 (pop token-list)))
	   (when (chat-string-p (car token-list))
	     (setf chat-string (pop token-list)))
	   (when token-list
	     (error 'parser-error :text "Unknown token encountered.")))
	  (t
	   (let ((fail-verb (car token-list)))
	     (if fail-verb
		 (error 'unknown-verb-error :text "Unknown verb" :verb fail-verb)
		 (error 'parser-error :text "Invalid input")))))
    (list verb noun-clause (list adverb-1 adverb-2 adverb-3) chat-string)))

(defun parse-noun-clause (token-list)
  "Generates the NOUN-CLAUSE list.
MULTIPLE RETURN VALUES: NOUN-CLAUSE list, and the remaining TOKEN-LIST"
  (let ((adverb nil)
	(preposition nil)
	(noun-phrase-2 nil))
    (multiple-value-bind (noun-phrase-1 token-list) (parse-noun-phrase token-list)
      (when (adverb-p (car token-list))
	(setf adverb (pop token-list)))
      (if (preposition-p (car token-list))
	  (progn (setf preposition (pop token-list))
		 (multiple-value-setq (noun-phrase-2 token-list) (parse-noun-phrase token-list))))
      (values (list preposition noun-phrase-1 noun-phrase-2) token-list))))

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
;;; Util
;;;

(defun extract-noun-from-possessive (word)
  "Nabs the actual noun out of a possessive."
  (when (possessive-p word)
    (car (cl-ppcre:split "'|'s" word))))

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


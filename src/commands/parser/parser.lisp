;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

;; sykosomatic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sykosomatic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
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
;;; - Takes a string-list, and returns an Astract Syntax Tree.

;; NOTE: Do I want to be able to accept two- or even three-verb sentences? It wouldn't be spammy,
;;       since the event system can handle putting delays between each action just fine.
;;
;; ABNF grammar - http://en.wikipedia.org/wiki/ABNF
;; ------------
;;
;; sentence =  chat-string
;; sentence =/ [adverb] verb [noun-clause] [adverb] [chat-string]
;;
;; noun-clause =/ [[[adverb] preposition] noun-phrase] [[[adverb] preposition] noun-phrase]
;;
;; noun-group =  noun-phrase [","] 0*(conjunction noun-phrase)
;;
;; noun-phrase =  pronoun
;; noun-phrase =/ [article] [cardinal] [adjective] noun
;; noun-phrase =/ [article] [ordinal] [adjective] \
;;                (noun / possessive noun-phrase)
;;
;; article = satisfies article-p
;; adjective = any unknown token that comes before a noun or a possessive
;; noun = anything before a preposition, a conjunction, an adverb, a chat-string, or a NIL
;; pronoun = satisfies pronoun-p
;; possessive-noun = satisfies possessive-p (['s] or [s'])
;; conjunction = satisfies conjunction-p (i.e. "and" "&" "," etc.)
;;
;; Goal AST - (verb noun-clause adverb-list chat-string)
;; -----------Where NOUN-CLAUSE is (preposition noun-phrase noun-phrase)
;; -----------Where NOUN-PHRASE is (list-of-objects)

;; Classes for AST
(defclass <sentence> ()
  ((verb        
    :accessor verb
    :initarg :verb
    :initform nil
    :type string)
   (noun-clause
    :accessor noun-clause
    :initarg :noun-clause
    :initform nil)
   (adverbs
    :accessor adverbs
    :initarg :adverbs
    :initform nil
    :type list)
   (chat-string
    :accessor chat-string
    :initarg :chat-string
    :initform nil
    :type string)))

(defmethod direct-objects ((sentence <sentence>))
  (direct-objects (noun-clause sentence)))

(defmethod indirect-objects ((sentence <sentence>))
  (indirect-objects (noun-clause sentence)))

(defmethod prepositions ((sentence <sentence>))
  (prepositions (noun-clause sentence)))

(defclass <noun-clause> ()
  ((direct-objects 
    :accessor direct-objects
    :initarg :direct-objects
    :initform nil
    :type list)
   (indirect-objects
    :accessor indirect-objects
    :initarg :indirect-objects
    :initform nil
    :type list)
   (prepositions 
    :accessor prepositions
    :initarg :prepositions
    :initform nil
    :type list)))
  
(defclass <noun-phrase> ()
  ((noun  
    :accessor noun
    :initarg :noun
    :initform nil
    :type string)
   (adjectives 
    :accessor adjectives
    :initarg :adjectives
    :initform nil
    :type list)
   (owns
    :accessor owns
    :initarg :owns
    :initform nil)))
 
;; AST Generation
(defun parse-string (string)
    "Parses a STRING that was entered by AVATAR and returns an Abstract Syntax Tree"
    (parse-sentence (string->token-list string)))

(defun parse-sentence (token-list)
  "Uses a TOKEN-LIST to generate an AST"
  (let ((sentence (make-instance '<sentence>)))
    (with-accessors ((verb verb)
		     (noun-clause noun-clause)
		     (adverbs adverbs)
		     (chat-string chat-string)) sentence
      (let ((adverb-1 nil)
	    (adverb-2 nil)
	    (adverb-3 nil)
	    (adverb-4 nil))	
	(when (adverb-p (car token-list))
	  (setf adverb-1 (pop token-list)))
	(cond ((and (verb-p "say")
		    (chat-string-p (car token-list)))
	       (setf verb "say")
	       (setf chat-string (remove-chat-string-tilde (pop token-list))))
	      ((verb-p (car token-list))
	       (setf verb (pop token-list))
	       (multiple-value-setq 
		   (noun-clause adverb-2 adverb-3 token-list) (parse-noun-clause token-list))
	       (when token-list
		 (when (adverb-p (car token-list))
		   (setf adverb-4 (pop token-list)))
		 (when (chat-string-p (car token-list))
		   (setf chat-string (remove-chat-string-tilde (pop token-list))))
		 (when token-list
		   (error 'parser-error 
			  :text "Input failed to parse (stuff left after finishing parse)."))))
	      (t
	       (let ((fail-verb (car token-list)))
		 (if fail-verb
		     (error 'parser-error :text (format nil "Unknown verb: ~a" fail-verb))
		     (error 'parser-error :text "Invalid input")))))
	(push adverb-4 adverbs)
	(push adverb-3 adverbs)
	(push adverb-2 adverbs)
	(push adverb-1 adverbs)))
    sentence))

(defun parse-noun-clause (token-list)
  "Generates the NOUN-CLAUSE list.
MULTIPLE RETURN VALUES: NOUN-CLAUSE list, a discovered ADVERB, and the remaining TOKEN-LIST"
  (let ((noun-clause (make-instance '<noun-clause>))
	(adverb1 nil)
	(adverb2 nil)
	(prep1 nil)
	(prep2 nil))
    (with-accessors ((dir-obj direct-objects)
		     (ind-obj indirect-objects)
		     (preps prepositions)) noun-clause
      (when (adverb-p (car token-list))
	(setf adverb1 (pop token-list)))
      (when (preposition-p (car token-list))
	(setf prep1 (pop token-list)))
      (multiple-value-setq (dir-obj token-list) (parse-noun-group token-list))
      (when (adverb-p (car token-list))
	(setf adverb2 (pop token-list)))
      (when (preposition-p (car token-list))
	(setf prep2 (pop token-list)))
      (multiple-value-setq (ind-obj token-list) (parse-noun-group token-list))
      (push prep2 preps)
      (push prep1 preps)
      (values noun-clause adverb1 adverb2 token-list))))

(defun parse-noun-group (token-list)
  "Parses a TOKEN-LIST into a LIST representing a NOUN GROUP (multiple noun 
phrases, joined by conjunctions) MULTIPLE RETURN VALUES: NOUN-GROUP and the 
REST of the TOKEN-LIST."
  (multiple-value-bind (noun-phrase token-list) (parse-noun-phrase token-list)
    (cond ((or (conjunction-p (car token-list))
	       (string-equal "," (car token-list)))
	   (when (and (string-equal "," (car token-list)) ;BAD COMMA, BAD
		      (conjunction-p (cadr token-list)))
	     (pop token-list))
	   (pop token-list)
	   (when (or (conjunction-p (car token-list))
		     (string-equal "," (car token-list)))
	     (error 'parser-error :text "Too many conjunctions."))
	   (multiple-value-bind (other-noun-phrases token-list) (parse-noun-group token-list)
	     (if noun-phrase
		 (values (append (list noun-phrase) other-noun-phrases) token-list)
		 (values nil token-list))))
	  
	  (t
	   (if noun-phrase
	       (values (list noun-phrase) token-list)
	       (values nil token-list))))))

(defun terminal-p (token)
  (or (possessive-p token)
      (preposition-p token)
      (chat-string-p token)
      (adverb-p token)
      (null token)
      (string-equal "," token)))

(defun parse-noun-phrase (token-list)
  "Parses a TOKEN-LIST into a LIST representing a NOUN PHRASE.
MULTIPLE RETURN VALUES: NOUN-PHRASE and REST of the TOKEN-LIST."
  (let ((noun-phrase (make-instance '<noun-phrase>)))
    (with-accessors ((noun noun)
		     (adjs adjectives)
		     (owns owns)) noun-phrase
      (cond ((or (preposition-p (car token-list))
		 (null (car token-list))
		 (chat-string-p (car token-list))
		 (adverb-p (car token-list)))
	     nil)
	    ((pronoun-p (car token-list))
	     (setf noun (pop token-list)))
	    (t
	     (when (article-p (car token-list))
	       (push (pop token-list) adjs))
	     (if (cardinal-number-p (car token-list))
		 (progn 
		   (loop until (or (preposition-p (cadr token-list))
				   (null (cadr token-list)))
		      do (push (pop token-list) adjs)
		      finally (setf noun (pop token-list))))
		 (progn
		   (loop until (or (possessive-p (car token-list))
					;(possessive-p (cadr token-list))
				   (conjunction-p (cadr token-list))				   
				   (string-equal "," (car token-list))
				   (string-equal "," (cadr token-list)) ;COMMA BAD!
				   (preposition-p (cadr token-list))
				   (chat-string-p (cadr token-list))
				   (adverb-p (cadr token-list))
				   (null (cadr token-list)))
		      do (push (pop token-list) adjs))
		   (if (possessive-p (car token-list))
		       (progn
			 (setf noun (extract-noun-from-possessive (pop token-list)))
			 (multiple-value-setq (owns token-list)
			   (parse-noun-phrase token-list)))
		       (setf noun (pop token-list)))))))

      (if noun
	  (values noun-phrase token-list)
	  (values nil token-list)))))

;;;
;;; Util
;;;

(defun remove-chat-string-tilde (chat-string)
  "Gets rid of the damn tilde."
  (cadr (cl-ppcre:split "'" chat-string)))

(defun extract-noun-from-possessive (word)
  "Nabs the actual noun out of a possessive."
  (when (possessive-p word)
    (car (cl-ppcre:split "'|'s" word))))

;; TODO: figure out a nice way to make a bunch of handleable errors like this.
(define-condition parser-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Condition signaled whenever some generic parsing error happens.")
  (:report (lambda (condition stream)
	     (format stream "~a" (text condition)))))

(defun prompt-user ()
  (format t "~~>")
  (read-line))

(defun test-the-parser ()
  "Runs a loop that asks for avatar input and returns whatever gets parsed. Quits on 'quit'."
  (let ((current-input (prompt-user)))
    (if (string-equal current-input "quit")
	(format t "Bye bye!")
	(progn
	  (let ((parse-tree (parse-string current-input)))
	    (format t "~%AST Generated: ~A~%" parse-tree))
	  (test-the-parser)))))

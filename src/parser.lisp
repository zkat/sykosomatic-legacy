;; Copyright 2008-2010 Kat Marchán

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
(in-package :sykosomatic)

(declaim (optimize debug))

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
  (awhen chat-string (format nil "'~a" it)))

(defun string->token-list (string)
  "Converts a STRING into a LIST of TOKEN-STRINGS."
  (let* ((com+chat (split-off-chat-string (preprocess-string string)))
         (commands (split-command-string (car com+chat))))
    (aif (cadr com+chat)
        (let ((chat-string (format-chat-string it)))
          (append commands (list chat-string)))
        commands)))

;;;
;;; Parser
;;;
;;; - Takes a string-list, and returns an AST.

(defclass abstract-sentence ()
  ((verb
    :accessor verb
    :initarg :verb
    :initform nil)
   (direct-object
    :accessor direct-object
    :initarg :direct-object
    :initform nil)
   (chat-string
    :accessor chat-string
    :initarg :chat-string
    :initform nil)))

;;;
;;; AST Generation
;;;

(defun parse-string (string)
  "Parses a STRING that was entered by AVATAR and returns an Abstract Syntax Tree"
  (parse-sentence (string->token-list string)))

;; ABNF grammar - http://en.wikipedia.org/wiki/ABNF
;; ------------
;;
;; sentence =  chat-string
;; sentence =/ [adverb] verb [noun-clause] [adverb] [chat-string]
;;
;; noun-clause =/ [[[adverb] preposition] noun-group] [[[adverb] preposition] noun-group]
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

(defun parse-sentence (token-list)
  "Uses a TOKEN-LIST to generate an AST"
  (let (adverb
        verb
        noun-clause
        chat-string)
    (flet ((maybe-parse-adverb ()
             (if adverb
                 (error 'parser-error :text "Too many adverbs.")
                 (when (adverbp (car token-list))
                   (setf adverb (pop token-list))))))
      (maybe-parse-adverb)
      (cond ((and (verbp "say")
                  (chat-string-p (car token-list)))
             (setf verb "say")
             (setf chat-string (prepare-chat-string (pop token-list))))
            ((verbp (car token-list))
             (setf verb (pop token-list))
             (multiple-value-setq (noun-clause adverb token-list)
               (parse-noun-clause token-list adverb))
             (when token-list
               (maybe-parse-adverb)
               (when (chat-string-p (car token-list))
                 (setf chat-string (prepare-chat-string (pop token-list))))
               (when token-list
                 (error 'parser-error
                        :text "Input failed to parse (stuff left after finishing parse)."))))
            (t
             (let ((fail-verb (car token-list)))
               (if fail-verb
                   (error 'parser-error :text (format nil "Unknown verb: ~a" fail-verb))
                   (error 'parser-error :text "Invalid input"))))))
    `(:sentence . ((:verb . ,verb)
                   (:adverb . ,adverb)
                   (:noun-clause . ,noun-clause)
                   (:chat-string . ,chat-string)))))

(defun parse-noun-clause (token-list adverb)
  "Generates the NOUN-CLAUSE list.
MULTIPLE RETURN VALUES: NOUN-CLAUSE list, a discovered ADVERB, and the remaining TOKEN-LIST"
  (flet ((maybe-parse-adverb ()
           (if adverb
               (error 'parser-error :text "Too many adverbs.")
               (when (adverbp (car token-list))
                 (setf adverb (pop token-list))))))
    (let (prep1 prep2 dir-obj ind-obj)
        (maybe-parse-adverb)
        (when (prepositionp (car token-list))
          (setf prep1 (pop token-list)))
        (multiple-value-setq (dir-obj token-list) (parse-noun-group token-list))
        (maybe-parse-adverb)
        (when (prepositionp (car token-list))
          (setf prep2 (pop token-list)))
        (multiple-value-setq (ind-obj token-list) (parse-noun-group token-list))
        (values `(:noun-clause
                  .
                  ((:direct-object
                    . (:prepositional-phrase
                       . ((:preposition . ,prep1)
                          (:object . ,dir-obj))))
                   (:indirect-object
                    . (:prepositional-phrase
                       . ((:preposition . ,prep2)
                          (:object . ,ind-obj))))))
                adverb))))

(defun parse-noun-group (token-list)
  "Parses a TOKEN-LIST into a LIST representing a NOUN GROUP (multiple noun
phrases, joined by conjunctions) MULTIPLE RETURN VALUES: NOUN-GROUP and the
REST of the TOKEN-LIST."
  (multiple-value-bind (noun-phrase token-list) (parse-noun-phrase token-list)
    (cond ((or (conjunctionp (car token-list))
               (string-equal "," (car token-list)))
           ;; NOTE: we can scrap the actual conjunction because we only accept 'and'
           (when (and (string-equal "," (car token-list)) ;BAD COMMA, BAD
                      (conjunctionp (cadr token-list)))
             (pop token-list))
           (pop token-list)
           (when (or (conjunctionp (car token-list))
                     (string-equal "," (car token-list)))
             (error 'parser-error :text "Too many conjunctions."))
           (multiple-value-bind (other-noun-phrases token-list)
               (parse-noun-group token-list)
             (if noun-phrase
                 (values (cons noun-phrase other-noun-phrases) token-list)
                 (values nil token-list))))
          (t
           (if noun-phrase
               (values (list noun-phrase) token-list)
               (values nil token-list))))))

(defun terminalp (token)
  (or (null token)
      (possessivep token)
      (prepositionp token)
      (chat-string-p token)
      (adverbp token)
      (string-equal "," token)))

;; TODO: This is a massive function, forks all over the place. Cut it up and make it more readable.
;; noun-phrase =  pronoun
;; noun-phrase =/ [article] [cardinal] [adjective] noun
;; noun-phrase =/ [article] [ordinal] [adjective] \
;;                (noun / possessive noun-phrase)

(defun parse-noun-phrase (token-list)
  (let ((state :pronoun)
        noun adjs amount ordinality owns)
    (loop
       (case state
         (:pronoun
          (let ((token (pop token-list)))
            (if (pronounp token)
                (return-from parse-noun-phrase
                  (values `(:noun-phrase
                            . ((:pronoun . ,token)))
                          token-list))
                (setf token-list (cons token token-list)
                      state :article))))
         (:article
          (let ((token (pop token-list)))
            (cond ((null token)
                   (error 'parser-error :text "What? All done?"))
                  ((articlep token)
                   (setf state :numerical))
                  (t
                   (push token token-list)
                   (setf state :numerical)))))
         (:numerical
          (let ((token (pop token-list)))
            (cond ((null token)
                   (error 'parser-error :text "Looking for a number, got nothing at all!"))
                  ((cardinal-number-p token)
                   (setf amount (extract-number token)
                         state :adjective))
                  ((ordinal-number-p token)
                   (setf ordinality (extract-number token)
                         state :adjective))
                  (t
                   (push token token-list)
                   (setf state :adjective)))))
         (:adjective
          ;; punting on adjectives for now.
          (setf state :noun))
         (:noun
          ;; Punting on possessives for now, too.
          (let ((token (pop token-list)))
            (cond ((null token)
                   (error 'parser-error :text "I NEEDED A NOUN HERE!"))
                  ((not (or (adverbp token)
                            (prepositionp token)
                            (chat-string-p token)))
                   (setf noun token)
                   (return-from parse-noun-phrase
                     (values `(:noun-phrase
                               . ((:noun . ,noun)
                                  (:adjectives . ,adjs)
                                  (:amount . ,amount)
                                  (:ordinality . ,ordinality)
                                  (:owns . ,owns))))))
                  (t (error 'parser-error :text "I'm so confused. Why am I here? Who are you?")))))))))

;;;
;;; Util
;;;

(defun last-char (string)
  (elt string (1- (length string))))

(defun prepare-chat-string (chat-string)
  (let ((string (remove-chat-string-tilde chat-string)))
    (if (char-equal #\" (last-char string))
        (subseq string 0 (1- (length string)))
        string)))

(defun remove-chat-string-tilde (chat-string)
  "Gets rid of the damn tilde."
  (cadr (cl-ppcre:split "'" chat-string :limit 2)))

(defun extract-number (word)
  ;; TODO - need some translations in vocabulary.lisp before these can be ported.
  (or #+nil(gethash word *cardinal-numbers*)
      #+nil(gethash word *ordinal-numbers*)
      (parse-integer word :junk-allowed t)))

(defun extract-noun-from-possessive (word)
  "Nabs the actual noun out of a possessive."
  (when (possessivep word)
    (car (cl-ppcre:split "'|'s" word))))

(define-condition parser-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Condition signaled whenever some generic parsing error happens.")
  (:report (lambda (condition stream)
             (format stream "Error parsing -- ~a" (text condition)))))

(defmethod print-object ((sentence abstract-sentence) stream)
  (print-unreadable-object (sentence stream :type t :identity t)
    (format stream "Verb: ~S, D.O.: ~S, Chat: ~S"
            (verb sentence)
            (direct-object sentence)
            (chat-string sentence))))

(defun test-the-parser ()
  "Runs a loop that asks for avatar input and returns whatever gets parsed. Quits on 'quit'."
  (loop for current-input = (read-line) do
       (cond ((string-equal current-input "quit")
              (format t "Bye bye!")
              (return-from test-the-parser))
             (t
              (handler-case
                  (format t "~&AST Generated: ~A~%" (parse-string current-input))
                (parser-error (e)
                  (format t "~&Got a parser error: ~A~%" e)))))))

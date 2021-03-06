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
  (cl-ppcre:all-matches-as-strings "[a-zA-Z0-9@/#$^&*'-]{1,}|," command-string))

(defun split-off-chat-string (string)
  "Takes a raw STRING and returns a LIST with COMMAND-STRING and CHAT-STRING"
  (cl-ppcre:split "^'| +'|\"" string :limit 2))

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
             (when (adverbp (car token-list))
               (if adverb
                   (error 'parser-error :text "Too many adverbs.")
                   (setf adverb (pop token-list))))))
      (maybe-parse-adverb)
      (cond ((and (verbp "say")
                  (car token-list)
                  (chat-string-p (car token-list)))
             (setf verb "say")
             (setf chat-string (prepare-chat-string (pop token-list))))
            ((verbp (car token-list))
             (setf verb (pop token-list))
             (when token-list
               (multiple-value-setq (noun-clause adverb token-list)
                 (parse-noun-clause token-list adverb)))
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
                   ,@(when adverb `((:adverb . ,adverb)))
                   ,@(when noun-clause `((:noun-clause . ,noun-clause)))
                   ,@(when chat-string `((:chat-string . ,chat-string)))))))

(defun parse-noun-clause (token-list adverb)
  "Generates the NOUN-CLAUSE list.
MULTIPLE RETURN VALUES: NOUN-CLAUSE list, a discovered ADVERB, and the remaining TOKEN-LIST"
  (flet ((maybe-parse-adverb ()
           (when (adverbp (car token-list))
             (if adverb
                 (error 'parser-error :text "Too many adverbs.")
                 (setf adverb (pop token-list))))))
    (let (prep1 prep2 noun-phrase-1 noun-phrase-2)
      (maybe-parse-adverb)
      (when (prepositionp (car token-list))
        (setf prep1 (pop token-list)))
      (multiple-value-setq (noun-phrase-1 token-list) (parse-noun-group token-list))
      (maybe-parse-adverb)
      (when token-list
        (when (prepositionp (car token-list))
          (setf prep2 (pop token-list)))
        (multiple-value-setq (noun-phrase-2 token-list) (parse-noun-group token-list)))
      (let ((tree nil))
        (when noun-phrase-2 (push `(:noun-group
                                    . (,@(when prep2 `((:preposition . ,prep2)))
                                         ,noun-phrase-2))
                                  tree))
        (when noun-phrase-1 (push `(:noun-group
                                    . (,@(when prep1 `((:preposition . ,prep1)))
                                         ,noun-phrase-1))
                                  tree))
        (values tree adverb token-list)))))

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
  (when token-list
   (let ((state :pronoun)
         article noun adjs amount ordinality owns)
     (loop
        (case state
          (:pronoun
           (awhen (pop token-list)
             (if (pronounp it)
                 (return-from parse-noun-phrase
                   (values `(:noun-phrase
                             . ((:pronoun . ,it)))
                           token-list))
                 (setf token-list (cons it token-list)
                       state :article))))
          (:article
           (awhen (pop token-list)
             (cond ((null it)
                    (error 'parser-error :text "What? All done?"))
                   ((articlep it)
                    (setf article it)
                    (setf state :numerical))
                   (t
                    (push it token-list)
                    (setf state :numerical)))))
          (:numerical
           (awhen (pop token-list)
             (cond ((null it)
                    (error 'parser-error :text "Looking for a number, got nothing at all!"))
                   ((cardinal-number-p it)
                    (setf amount (extract-number it)
                          state :adjective))
                   ((ordinal-number-p it)
                    (setf ordinality (extract-number it)
                          state :adjective))
                   (t
                    (push it token-list)
                    (setf state :adjective)))))
          (:adjective
           (loop until (or (possessivep (car token-list))
                           (conjunctionp (car token-list))
                           (string-equal "," (car token-list))
                           (null (second token-list))
                           ;; looking ahead...
                           (and (second token-list)
                                (or (conjunctionp (second token-list))
                                    (string-equal "," (second token-list))
                                    (prepositionp (second token-list))
                                    (chat-string-p (second token-list))
                                    (adverbp (second token-list)))))
              do (push (pop token-list) adjs))
           (setf state :noun))
          (:noun
           (let ((it (pop token-list)))
             (cond ((and (null it)
                         (or adjs amount ordinality owns))
                    (error 'parser-error :text "I NEEDED A NOUN HERE!"))
                   ((and (stringp it)
                         (not (or (adverbp it)
                                  (prepositionp it)
                                  (chat-string-p it))))
                    (if (possessivep it)
                        (progn
                          (setf noun (extract-noun-from-possessive it))
                          (multiple-value-setq (owns token-list)
                            (parse-noun-phrase token-list)))
                        (setf noun it))
                    (return-from parse-noun-phrase
                      (values `(:noun-phrase
                                . (,@(when article `((:article . ,article)))
                                     (:noun . ,noun)
                                     ,@(when adjs `((:adjectives . ,adjs)))
                                     ,@(when amount `((:amount . ,amount)))
                                     ,@(when ordinality `((:ordinality . ,ordinality)))
                                     ,@(when owns `((:possesses . ,owns)))))
                              token-list)))
                   (t (when it (push it token-list))
                      (return-from parse-noun-phrase (values nil token-list)))))))))))

;;;
;;; Util
;;;

(defun last-char (string)
  (if (plusp (length string))
      (elt string (1- (length string)))
      #\space))

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
  ((text :initarg :text :accessor text))
  (:report (lambda (e s)
             (format s "~A" (text e)))))

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

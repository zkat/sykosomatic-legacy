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
  (when chat-string
    (format nil "'~a" chat-string)))

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

(defun parse-sentence (tokens)
  (when (null tokens)
    (error 'parser-error :text "Nothing to parse?"))
  (let ((sentence (make-instance 'abstract-sentence))
        (state :verb))
    (loop
       (case state
         (:verb
          (let ((verb (pop tokens)))
            (cond ((verbp verb)
                   (setf (verb sentence) verb
                         state :chat-string))
                  ((chat-string-p verb)
                   (setf (verb sentence) "say"
                         state :chat-string))
                  (t
                   (error 'parser-error :text (format nil "Invalid verb: ~S" verb))))))
         (:chat-string
          (cond ((null tokens)
                 (return-from parse-sentence sentence))
                ((chat-string-p (car tokens))
                 (setf (chat-string sentence) (prepare-chat-string (pop tokens)))
                 (return-from parse-sentence sentence))
                (t
                 (setf state :direct-object))))
         (:direct-object
          (if (direct-object sentence)
              (error 'parser-error :text "Too many direct objects.")
              (setf (direct-object sentence) (pop tokens)
                    state :chat-string)))))))

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

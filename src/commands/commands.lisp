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

;; commands.lisp
;;
;; Currently, it's an amalgamation of a sort of binder, vocabulary handler, and several avatar
;; functions, along with commands to execute them. Very nasty stuff.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic.core)

;;;
;;; Executor
;;;

;; NOTE: I should probably make events be a lower-level thing in sykosomatic, so more things
;;       can be handled through the event processor. This would be ideal, and elegant.
(defun process-avatar-input (avatar input)
  "Generates an event out of processed avatar input. Takes care of handling any conditions
coming from the parser and/or binder."
  (handler-case
      (let ((payload (process-command avatar input)))
	(make-event payload))
    (parser-error () (write-to-target avatar "parser error~%"))))

;; FIXME: This does too much.
(defun process-command (avatar input)
  "Calls the parser on input, grabs a function out of the AST, presumably binds stuff,
then proceeds to return a lambda it builds which will serve as a payload for an event."
  (let* ((ast (parse-string input))
	 (function (bind-verb (verb ast))))
    (when (and ast function)
      #'(lambda () (funcall function avatar ast)))))

;;;
;;; Base functions
;;;

(defun present-tense (verb)
  "Converts a verb in imperative form to its present tense form."
  (let ((verb-ending (elt verb (- (length verb) 1))))
    (cond ((string-equal verb-ending "h")
	   (concatenate 'string verb "es"))
	  ((string-equal verb-ending "y")
	   (let ((verb (string-right-trim "y" verb))) ;NOTE: This murders verbs that end in yy...y.
	     (concatenate 'string verb "ies")))
	  (t
	   (concatenate 'string verb "s")))))

;;;
;;; Base Commands
;;;

(defgeneric game-action-emote (entity ast)
  (:documentation "Outputs the verb in action form. No other actions take place."))

(defmethod game-action-emote ((avatar <avatar>) ast)
  "If the emote has to do with a avatar, write to that avatar, as well as anyone in room"
  (write-to-target avatar (format-for-caller ast))
  (write-to-others-in-room avatar (format-for-others-in-room (name avatar) ast)))

(defgeneric game-action-look (entity ast)
  (:documentation "Represents the action of ENTITY looking, optionally at DIRECT-OBJECT."))

(defmethod game-action-look ((avatar <avatar>) ast)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns AVATAR LOCATION's DESC instead"
  (let ((noun-phrase (cadr ast)))
    (let* ((current-room (location avatar))
	   (target-string (car (car noun-phrase)))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (write-to-target avatar "~a" (desc target))
	  (write-to-target avatar "~a" (desc current-room))))))

;;;
;;; String Generation
;;;
;;; NOTE: These should probably expect to be either more informed, or less informed.
;;;       Meaning, it's possible that they might end up needing information from the binder.
;;;       If not, then they should possibly be a little blinder than they are now and become
;;;       more low-level tools for functions that -are- binder-aware.

(defun format-noun-group (noun-group)
  "Takes a noun group and formats it in 'a, b, and c' form."
  (when noun-group
   (format nil "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}" (mapcar #'format-noun-phrase noun-group))))

(defun format-noun-phrase (noun-phrase)
  "Converts a noun phrase into a proper descriptors+noun descriptor."
  (noun noun-phrase))

(defun format-for-caller (ast)
  "Formats a sentence into its second-person form (for the caller)."
  (with-accessors ((verb verb) (dir-objs direct-objects) (ind-objs indirect-objects) 
		   (preps prepositions) (advs adverbs) (chat chat-string))
      ast
    (format nil 
	    "~@(~@[~a, ~]you~) ~a~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[, \"~a\"~].~%" 
	    (first advs) verb (second advs)
	    (first preps) (format-noun-group dir-objs)
	    (third advs) (second preps)
	    (format-noun-group ind-objs)
	    (fourth advs) chat)))

(defun format-for-others-in-room (caller-name ast)
  "Formats a sentence into its general third-person form."
  (with-accessors ((verb verb) (dir-objs direct-objects) (ind-objs indirect-objects) 
		   (preps prepositions) (advs adverbs) (chat chat-string))
      ast
    (format nil
	    "~@(~@[~a, ~]~)~a ~a~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[, \"~a\"~].~%" 
	    (first advs) caller-name (present-tense verb) (second advs)
	    (first preps) (format-noun-group dir-objs)
	    (third advs) (second preps)
	    (format-noun-group ind-objs)
	    (fourth advs) chat)))

;; TODO - right approach? Maybe not.
(defun format-for-target (target-name ast)
  "Formats a sentence so that the target-name is replaced with 'you' or 'your' or such. (uh oh)"
  (format nil "foo"))

;;;
;;; Utils
;;;

;; nil
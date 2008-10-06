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
(in-package #:sykosomatic)

;;;
;;; Executor
;;;
(defun process-avatar-input (avatar input)
  (handler-case
      (let ((payload (process-command avatar input)))
	(make-event payload))
    (parser-error () (write-to-target avatar "parser error"))))

(defun process-command (avatar input)
  (let* ((ast (parse-string input))
	 (function (bind-verb (verb ast))))
    #'(lambda () (funcall function avatar ast))))

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
;;; Utils
;;;

;;NOTE: This should probably go elsewhere.
(defun write-to-others-in-room (caller format-string &rest format-args)
  "Works like FORMAT, writing its arguments to everyone in CALLER's location, except to CALLER."
  (when (location caller)
    (let ((other-avatars (remove caller (get-avatars (location caller)))))
     (loop for avatar in other-avatars
	do (write-to-target avatar format-string format-args)))))

;;; string generation
(defun format-noun-phrase (noun-phrase)
  (when (noun (car noun-phrase))
   (format nil "~{~#[~;~a~;~a and ~a~:;~@{~a~#[~;, and ~:;, ~]~}~]~}" (generate-names noun-phrase))))

(defun generate-names (noun-phrase)
  (mapcar #'noun noun-phrase))

(defun format-for-caller (ast)
  (with-accessors ((verb verb) (dir-objs direct-objects) (ind-objs indirect-objects) 
		   (preps prepositions) (advs adverbs) (chat chat-string))
      ast
    (format nil 
	    "~@[~a, ~]You ~a~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[, \"~a\"~].~%" 
	    (first advs) verb (second advs)
	    (first preps) (format-noun-phrase dir-objs)
	    (third advs) (second preps)
	    (format-noun-phrase ind-objs)
	    (fourth advs) chat)))

(defun format-for-others-in-room (caller-name ast)
  (with-accessors ((verb verb) (dir-objs direct-objects) (ind-objs indirect-objects) 
		   (preps prepositions) (advs adverbs) (chat chat-string))
      ast
    (format nil
	    "~@[~a, ~]~a ~a~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[ ~a~]~@[, \"~a\"~].~%" 
	    (first advs) caller-name (present-tense verb) (second advs)
	    (first preps) (format-noun-phrase dir-objs)
	    (third advs) (second preps)
	    (format-noun-phrase ind-objs)
	    (fourth advs) chat)))


;;; database
(defun add-verb (string function)
  "Associates STRING with FUNCTION and adds the new verb to *VERBS*"
  (setf (gethash string *verbs*) function)
  (save-vocabulary))

(defun remove-verb (string)
  "Removes the VERB that corresponds to STRING from *VERBS*"
  (remhash string *verbs*)
  (save-vocabulary))

(defun refresh-verb (string function)
  "Associates STRING with FUNCTION and adds it to *VERBS*,
removing all previous associations with STRING"
  (remove-verb string)
  (add-verb string function)
  (save-vocabulary))

(defun add-emote (string)
  (add-verb string #'game-action-emote))

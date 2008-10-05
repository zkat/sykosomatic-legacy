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

;; TODO
(defun execute-command (avatar string)
  "Takes a STRING and EXECUTES the appropriate command within AVATAR's context."
  t)

;;;
;;; Base functions
;;;

(defun present-tense (verb)
  "Converts a verb in imperative form to its present tense form."
  (let ((verb-ending (elt verb (- (length verb) 1))))
    (cond ((string-equal verb-ending "h")
	   (concatenate 'string verb "es"))
	  ((string-equal verb-ending "y")
	   (let ((verb (string-right-trim "y" verb))) ;could cause problems.
	     (concatenate 'string verb "ies")))
	  (t 
	   (concatenate 'string verb "s")))))

;;;
;;; Base Commands
;;;

;; TODO - What should a call to a defcommand/verb macro look like?
;;
;; This is what all commands receive as argument:
;; (<caller-object> ast)

;; NOTE: Here's an example of how commands should be handled. Any object that needs a special action
;; performed upon it should define either an overriding method, or something to do before calling
;; (call-next-method)

;; NOTE: The defverb macro should provide a pretty simple, uber-abstracted interface for
;;       defining new game commands. It should also try to remain as flexible as possible.
;;       It should also handle adding the command to the *verbs* hash table, and redifining them
;;       as necessary.

;; What does a verb have to do?
;; 1. Apply a certain effect to several items
;; 2. spit out a message to all relevant targets
;; 3. manage multiple targets, and multiple effects per target.
;;
;; How are these achieved?
;; 1, 3, write methods that specifically act upon single targets, map them to the targets
;; 2. define 1st person, 2nd person, and 3rd person messages as necessary (only defined ones get spit out)
;;

;; TODO
(defun write-to-others-in-room (caller format-string &rest format-args)
  (let ((others (get-avatars (location caller))))
    (apply #'write-to-target)))

(defgeneric game-action-emote (entity ast)
  (:documentation "Outputs the verb in action form. No other actions take place."))

(defmethod game-action-emote ((avatar <avatar>) ast)
  "If the emote has to do with a avatar, write to that avatar, as well as anyone in room"
  (with-accessors ((verb verb) (dir-objs direct-objects) (ind-objs indirect-objects)) ast
    (write-to-target avatar "You ~a.~%" verb)
   (write-to-others-in-room "~a ~a.~%" (name avatar) (present-tense verb))))

(defgeneric game-action-look (entity ast)
  (:documentation "Represents the action of ENTITY looking, optionally at DIRECT-OBJECT."))

(defmethod game-action-look ((avatar <avatar>) ast)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns AVATAR LOCATION's DESC instead"
  (let ((noun-phrase (cadr ast)))
    (let* ((current-room (location avatar))
	   (target-string (car (car noun-phrase)))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (write-to-avatar avatar "~a" (desc target))
	  (write-to-avatar avatar "~a" (desc current-room))))))


;;;
;;; Utils
;;;

(defun add-verb (string function)
  "Associates STRING with FUNCTION and adds the new verb to *VERBS*"
  (setf (gethash string *verbs*) function))

(defun remove-verb (string)
  "Removes the VERB that corresponds to STRING from *VERBS*"
  (remhash string *verbs*))

(defun refresh-verb (string function)
  "Associates STRING with FUNCTION and adds it to *VERBS*,
removing all previous associations with STRING"
  (remove-verb string)
  (add-verb string function))

(defun add-emote (string)
  (add-verb string #'pc-emote))

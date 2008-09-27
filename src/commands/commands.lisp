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
;; Currently, it's an amalgamation of a sort of binder, vocabulary handler, and several player
;; functions, along with commands to execute them. Very nasty stuff.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Executor
;;;

;; TODO
(defun execute-command (player string)
  "Takes a STRING and EXECUTES the appropriate command within PLAYER's context."
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

(defun breakdown-ast (ast)
  "Returns the desired contents of AST"
  t)

;;;
;;; Base Commands
;;;

;; TODO - What should a call to a defmacro command look like?
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

(defgeneric action-emote (entity ast)
  (:documentation "Outputs the verb in action form. No other actions take place."))

(defmethod action-emote ((player <player>) ast)
  "If the emote has to do with a player, write to that player, as well as anyone in room" ;; uh.. no?
  (let ((verb (verb ast)))
    (write-to-player player "You ~a.~%" verb)
   (write-to-others-in-room "~a ~a.~%" (name player) (present-tense verb))))

(defgeneric action-look (entity ast)
  (:documentation "Represents the action of ENTITY looking, optionally at DIRECT-OBJECT."))

(defmethod action-look ((player <player>) ast)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let ((noun-phrase (cadr ast)))
    (let* ((current-room (location player))
	   (target-string (car (car noun-phrase)))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (write-to-player player "~a" (desc target))
	  (write-to-player player "~a" (desc current-room))))))

(defun pc-quit (player ast)
  "Takes care of quitting the game."
  (disconnect-player player))

(defun pc-examine (player ast)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let ((noun-phrase (cadr ast)))
    (let* ((current-room (location player))
	   (target-string (car (car noun-phrase)))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (progn (write-to-player player "You begin to examine ~a.~%" (name target))
		 (sleep 0.8)
		 (write-to-player player "~a" (desc-long target)))
	  (progn (write-to-player player "You begin to examine ~a.~%" (name current-room))
		 (sleep 0.8)
		 (write-to-player player "~a" (desc-long current-room)))))))

;; NOTE: OMG CODE REPETITION :< --Kat

(defun pc-go (player ast)
  "Moves PLAYER in DIRECTION."
  (let ((direction (car (car (cadr ast)))))
    (let ((curr-room (location player)))
      (if curr-room
	  (let ((exit (assoc direction
			     (exits curr-room) :test #'string-equal)))
	    (if exit
		(let ((next-room
		       (next-room (cdr exit))))
		  (if next-room
		      (progn
			(put-entity player next-room)
			(write-to-player player "You begin to enter ~a." (name (cdr exit)))
			(sleep 0.7)
			(write-to-player player "~%~a" (desc (location player)))
			(sleep 0.7))
		      (write-to-player player "There's nowhere to go through there.")))
		(write-to-player player "No exit in that direction.")))
	  (write-to-player player "Player can't move. He isn't anywhere to begin with!")))))

(defun pc-cardinal-move (player ast)
  "Moves PLAYER in DIRECTION."
  (let ((direction (car ast))) ;;the emote itself is the direction.
    (let ((curr-room (location player)))
      (if curr-room
	  (let ((exit (assoc direction
			     (exits curr-room) :test #'string-equal)))
	    (if exit
		(let ((next-room (next-room
				  (cdr exit))))
		  (if next-room
		      (progn
			(put-entity player next-room)
			(write-to-player player "You begin to enter ~a." (name (cdr exit)))
			(sleep 0.7)
			(write-to-player player "~%~a" (desc (location player)))
			(sleep 0.7))
		      (write-to-player player "There's nowhere to go through there.")))
		(write-to-player player "No exit in that direction.")))
	  (write-to-player player "Player can't move. He isn't anywhere to begin with!")))))

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

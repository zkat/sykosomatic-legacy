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

(defun execute-command (player string)
  "Takes a STRING and EXECUTES the appropriate command within PLAYER's context."
  (let ((sexp (string->sexp player string)))
    (if (functionp (car sexp))
	(apply (car sexp) (cdr sexp)))))

;;;
;;; Base Commands
;;;

;; TODO - Keep an eye out for a possible defcommand macro.
;;
;; This is what all commands receive as argument:
;; (<player> (emote rest-of-predicate adverbs chat-string))

(defun pc-emote (player ast)
  "Emotes an EMOTE-STRING."
  (let ((emote (car ast)))
    (write-to-player player "You ~a.~%" emote)))

(defun pc-quit (player ast)
  "Takes care of quitting the game."
  (disconnect-player player))

(defun pc-look (player ast)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let ((noun-phrase (cadr ast)))
    (let* ((current-room (location player))
	   (target-string (car (car noun-phrase)))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (write-to-player player "~a" (desc target))
	  (write-to-player player "~a" (desc current-room))))))

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

(defun refresh-verb (string function)
  "Associates STRING with FUNCTION and adds it to *VERBS*,
removing all previous associations with STRING"
  (remove-verb string)
  (add-verb string function))

(defun add-verb (string function)
  "Associates STRING with FUNCTION and adds the new verb to *VERBS*"
  (pushnew (cons string function) *verbs*))

(defun remove-verb (string)
  "Removes the VERB that corresponds to STRING from *VERBS*"
  (setf *verbs*
	(delete
	 (assoc string *verbs* :test #'string-equal)
	 *verbs*)))

(defun add-emote (string)
  (add-verb string #'pc-emote))

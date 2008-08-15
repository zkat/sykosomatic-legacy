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

(in-package #:sykosomatic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;======================================  User Commands  =======================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Sexy Builder ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; AST goes in, sexp goes out. (it builds s-exps, so it's sexy)
;; -----------------------------------------------
;; Goal AST - (emote rest-of-sentence adverb chat-string) ;;this will be expanded further.
;; ----------Where rest-of-sentence is ((noun-phrase) &optional (noun-phrase))
;; ---------------Where noun-phrase is ((descriptors) &optional (descriptors))
;; ---------------------where descriptors is ("noun" &rest "adjectives, articles, etc")
;; -----------------------------------------------
;
(defun string->function (string)
  "Checks if STRING is a VERB. Returns a FUNCTION."
  (cdr (assoc string *verbs* :test #'string-equal)))

(defun parse-tree->sexp (player tree)
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (let ((verb (string->function (car tree)))
	(emote (car tree))
	(rest-of-sentence (cadr tree))
	(chat-string (fourth tree))
	(adverb (third tree)))
    (list verb player (list emote rest-of-sentence adverb chat-string))))

(defun string->sexp (player string)
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp player (parse-string string)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ EXECUTOR!! ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun execute-command (player string)
  "Takes a STRING and EXECUTES the appropriate command within PLAYER's context."
  (let ((sexp (string->sexp player string)))
    (if (functionp (car sexp))
	(apply (car sexp) (cdr sexp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~ Functions ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO - Keep an eye out for a possible defcommand macro.
;;
;; This is what all commands receive as argument:
;; (<player> (emote rest-of-predicate adverbs chat-string))
;
(defun write-to-player (player format-string &rest format-args)
  (let ((player-client (current-client player)))
    (apply #'write-to-client player-client format-string format-args)))

(defun disconnect-player (player)
  (disconnect-client (current-client player)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~ Utils ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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

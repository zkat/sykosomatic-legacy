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
;; !!! Working on parser... Expect breakage.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~ Functions ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO - Maybe add a defalias command? Maybe this should be done at a different level. I'll stick to go for now.
;; TODO - Keep an eye out for a possible defcommand macro.
;;
;; This is what all commands receive as argument:
;; (<player> (emote noun-phrase adverb emote-string))
;

;; !!! TODO - These no longer work. Fix them up so they can actually use the nice little tree we made.
(defun pc-emote (player ast)
  "Emotes an EMOTE-STRING."
  (let ((emote (car ast)))
    (format t "You ~a." emote)))

(defun pc-look (player &rest ast)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let ((player (first ast))
	(noun-phrase (second ast)))
    (let* ((current-room (location player))
	   (target-string (first noun-phrase))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (format t "~a" (desc target))
	  (format t "~a" (desc current-room))))))

(defun pc-examine (player &rest ast)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let ((player (first ast))
	(noun-phrase (second ast)))
    (let* ((current-room (location player))
	   (target-string (first noun-phrase))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (progn (format t "You begin to examine ~a.~%" (name target))
		 (sleep 0.8)
		 (format t "~a" (desc-long target)))
	  (progn (format t "You begin to examine ~a.~%" (name current-room))
		 (sleep 0.8)
		 (format t "~a" (desc-long current-room)))))))


(defun pc-direction-go (player &rest ast)
  "Moves PLAYER in DIRECTION."
  (let ((player (car ast))
	(direction (third ast)))
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
			(format t "You begin to enter ~a." (name (cdr exit)))
			(sleep 0.7) ;;removing while I test. This should go in later, though.
			(format t "~%~a" (desc (location player)))
			(sleep 0.7))
		      (format t "There's nowhere to go through there.")))
		(format t "No exit in that direction.")))
	  (format t "Player can't move. He isn't anywhere to begin with!")))))

(defun pc-go (player &rest ast)
  "Moves PLAYER in DIRECTION."
  (let ((player (car ast))
	(noun-phrase (cadr ast)))
    (let ((curr-room (location player)))
      (if curr-room
	  (let* ((direction (car noun-phrase))
		 (exit (assoc direction
			      (exits curr-room) :test #'string-equal)))
	    (if exit
		(let ((next-room (next-room 
				  (cdr exit))))
		  (if next-room 
		      (progn 
			(put-entity player next-room)
			(format t "You begin to enter ~a." (name (cdr exit)))
			(sleep 0.7) ;;removing while I test. This should go in later, though.
			(format t "~%~a" (desc (location player)))
			(sleep 0.7))
		      (format t "There's nowhere to go through there.")))
		(format t "No exit in that direction.")))
	  (format t "Player can't move. He isn't anywhere to begin with!")))))

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

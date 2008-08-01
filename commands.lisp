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
;;
(defun emote (&rest args)
  "Emotes an EMOTE-STRING."
  (let ((emote (third args)))
    (format t "You ~a." emote)))

(defun look (&rest args)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let ((player (first args))
	(noun-phrase (second args)))
    (let* ((current-room (location player))
	   (target-string (first noun-phrase))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (format t "~a" (desc target))
	  (format t "~a" (desc current-room))))))

(defun examine (&rest args)
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let ((player (first args))
	(noun-phrase (second args)))
    (let* ((current-room (location player))
	   (target-string (first noun-phrase))
	   (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
      (if target
	  (format t "~a" (desc-long target))
	  (format t "~a" (desc-long current-room))))))

;; (defgeneric move (entity direction)
;;   (:documentation "Moves ENTITY in DIRECTION"))

(defun move (&rest args)
  (let ((player (first args))
	(noun-phrase (second args)))
    (let ((curr-room (location player)))
      (if curr-room
	  (let* ((direction (car noun-phrase))
		 (exit (assoc direction
			      (exits curr-room) :test #'string-equal)))
	    (if exit
		(let ((next-room (next-room 
				  (cdr exit))))
		  (if next-room 
		      (put-entity player next-room)
		      (format t "No exit in that direction~%")))
		(format t "No exit in that direction.~%")))
	  (format t "Player can't move. He isn't anywhere to begin with!~%")))))
  
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
  (pushnew string *emotes*))

(defun remove-emote (string)
  (setf *emotes*
	(delete
	 (find string *emotes* :test #'string-equal)
	 *emotes*)))


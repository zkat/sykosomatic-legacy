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

(in-package :sykosomatic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;====================================== Player class ==========================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defclass <player> (<mobile>)
  ((desc
    :initform "This is you.")
   (desc-long
    :initform "This is you. You are quite handsome. ;)")
   (player-id
    :initarg :player-id
    :initform (incf *player-ids*)
    :reader player-id
    :documentation "A unique player id.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;================================= Player-related functions ===================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~ Player Generation ~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun new-player ()
  "RETURNS a new PLAYER after initializing the <player> object"
  (let ((player (make-instance '<player>)))
    (with-accessors ((name name)) player
      (setf name (format nil "Player #~a" (player-id player))))
    player))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~  User Commands  ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; !!! Working on parser... Expect breakage.
(defun look (player &optional (noun-phrase nil))
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let* ((current-room (location player))
	 (target-string (first noun-phrase))
	 (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
    (if target
	(format t "~a" (desc target))
	(format t "~a" (desc current-room)))))

(defun examine (player &optional (noun-phrase nil))
  "Returns OBJECT's DESC. If no OBJECT is passed, it returns PLAYER LOCATION's DESC instead"
  (let* ((current-room (location player))
	 (target-string (first noun-phrase))
	 (target (find target-string (contents current-room) :key #'name :test #'string-equal)))
    (if target
	(format t "~a" (desc-long target))
	(format t "~a" (desc-long current-room)))))

(defgeneric move (entity direction)
  (:documentation "Moves ENTITY in DIRECTION"))

(defmethod move ((player <player>) noun-phrase)
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
    (format t "Player can't move. He isn't anywhere to begin with!~%"))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~  User Interaction ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun prompt-user ()
  "Prompts the user for input, and returns a string."
  (format t "~%~%-> ")
  (read-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Load/Save ~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defmethod obj->file ((player <player>) path)
  (cl-store:store player (ensure-directories-exist
			  (merge-pathnames
			   (format nil "player-~a.player" (player-id player))
			   path))))

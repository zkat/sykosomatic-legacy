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

;; player.lisp
;;
;; Holds the <player> class. Also contains some functions for character loading/saving.
;; Some management functions exist, too, but those could be moved out.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Player vars
;;;

(defvar *players* nil
  "List of existing players. Nice as a backup of the ones existing in accounts.")

(defvar *max-player-id* 0
  "Contains the highest available player-id")

(defvar *player-id-lock* (bordeaux-threads:make-lock)
  "This lock should be held by anything that wants to manipulate *max-room-id*")

;;;
;;; Player class
;;;

(defclass <player> (<mobile>)
  ((name
    :initform "NoNamePlayer")
   (player-id
    :initarg :player-id
    :initform (with-lock-held (*player-id-lock*) (incf *max-player-id*))
    :reader player-id
    :documentation "A unique player id.")
   (current-client
    :initarg :current-client
    :initform nil
    :accessor current-client
    :documentation "The <client> currently associated with this <player>"))
  (:documentation ""))

;;;
;;; Info
;;;

(defun player-p (obj)
  "Returns T if a given OBJ is an instance of <PLAYER>."
  (eq (class-name (class-of obj))
      '<player>))

(defun get-players (room)
  "Fetches a list of players currently in ROOM."
  (remove-if-not #'player-p (contents room)))

;;;
;;; Player functions
;;;

(defmethod write-to-target ((player <player>) format-string &rest format-args)
  "Sends output to a player."
  (let ((player-client (current-client player)))
    (if player-client
	(apply #'write-to-client player-client format-string format-args)
	(error "Player is not connected."))))

;; NOTE: Should the player be saved, too? Anything else?
(defun disconnect-player (player)
  "Disconnects the given player from the game."
  (disconnect-client (current-client player))
  (setf (current-client player) nil))

;;;
;;; Load/Save
;;;

;;; Save

(defmethod obj->file ((player <player>) path)
  (cl-store:store player (ensure-directories-exist
			  (merge-pathnames
			   (format nil "player-~a.player" (player-id player))
			   path))))

(defun save-players ()
  "Saves all players in *players* to individual files."
  (obj-list->files-in-dir *players* *players-directory*))

;;; Load

(defun restore-max-player-id ()
  "Loads the highest player-id."
  (let ((player-ids (or (mapcar #'player-id *players*) '(0))))
    (with-lock-held (*player-id-lock*)
      (setf *max-player-id* 
	    (apply #'max player-ids)))))

;; NOTE: This breaks if tries to load an object that was created with an obsolete class.(as it should)
(defun load-players ()
  "Takes care of loading all players into the *players* list"
  (setf *players* (files-in-path->obj-list *players-directory* "player"))
  (restore-max-player-id))

;;; Testing

(defun new-test-player ()
  "RETURNS a new PLAYER with its player-id as its name."
  (let ((player (make-instance '<player>)))
    (setf (name player) (format nil "Player~d" (player-id player)))
    player))

(defun reset-max-player-id ()
  "Sets the highest player-id to 0."
  (with-lock-held (*player-id-lock*)
    (setf *max-player-id* 0)))

(defun generate-test-players (num-players)
  "Returns a LIST containing NUM-PLAYERS generic instances of <player>."
  (loop 
     for i upto (1- num-players)
     collect (new-player)))

;;; Useless? It's more likely than you think.
(defun make-players-from-file (file)
  "Generates players from a raw text FILE. Returns a list of player objects." 
  (let ((players (with-open-file (in file)
		  (loop for line = (read in nil)
		     while line
		     collect line))))
    (mapcar #'eval players)))

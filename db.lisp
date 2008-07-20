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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;========================================== Database ==========================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(in-package #:sykosomatic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Variables ~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defvar *rooms* nil)
(defvar *players* nil)
(defvar *objects* nil)

(defvar *current-player* nil)
(defvar *player-ids* 0)
(defvar *room-ids* 0)

(defvar *vocabulary* nil)
(defvar *commands* nil)
(defvar *directions* '("north" "south" "east" 
		       "west" "northeast" "northwest" 
		       "southeast" "southwest" "up" 
		       "down" "enter"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~ Load/Save ~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; The following section can either be an alternative to, used alongside their globals-using versions,
;; or removed altogether. I'm not sure whan to do about it right now. Using both might cause
;; conflicts, but being able to individually save rooms/players is a plus.
;;
;; It's worth noting that it's much slower at saving the entire blob, and makes quite a few files.
;; The tradeoff is that I can incrementally save rooms (and their contents), with the potential to
;; swap out data to the hard drive. This means a persistent world where items and other things don't poof :)
;
(defgeneric write-object-to-file (object path)
  (:documentation "Saves OBJECT to a file."))

(defun objects-to-files (object-list path)
  "Saves all OBJECTS in OBJECT-LIST into files within PATH"
  (loop for object in object-list
       do (write-object-to-file object path)))

(defun get-object-from-file (filepath)
  (cl-store:restore filepath))

(defun get-objects-from-directory (path)
  (let ((files (directory (merge-pathnames "*.*" path))))
    (loop for file in files
	 collect (cl-store:restore file))))

;; Begin evil globals-users. Want to remove. :(
;; -----------------------------------------------
;
(defun save-db (db filename)
  "Saves the provided DB into FILENAME in the game's db/ dir."
  (cl-store:store db 
		  (ensure-directories-exist 
		   (merge-pathnames 
		    filename 
		    *db-directory*))))

(defun get-db-from-file (filename)
  "Gets the contents of FILENAME and returns them in a setf-able format."
  (cl-store:restore
   (merge-pathnames filename *db-directory*)))

(defun save-all-db ()
  "Saves all the database variables to file."
  (save-db *player-ids* #P"pid.db")
  (save-db *room-ids* #P"rid.db")
  (save-db *rooms* #P"rooms.db")
  (save-db *players* #P"players.db")
  (save-db *objects* #P"objects.db")
  (save-db *vocabulary* #P"vocabulary.db")
  (save-db *commands* #P"commands.db"))

(defun load-all-db ()
  "Loads everything from the db/ directory."
  (setf *player-ids*(get-db-from-file #P"pid.db"))
  (setf *room-ids*(get-db-from-file #P"rid.db"))
  (setf *rooms* (get-db-from-file #P"rooms.db"))
  (setf *players* (get-db-from-file #P"players.db"))
  (setf *objects* (get-db-from-file #P"objects.db"))
  (setf *vocabulary* (get-db-from-file #P"vocabulary.db"))
  (setf *commands* (get-db-from-file #P"commands.db")))

(defun write-rooms-to-files ()
  "Bastard child of all evil."
  (loop for room in *rooms*
     do (write-object-to-file room)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~ Utilities ~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun reset-player-ids ()
  (setf *player-ids* 0))

(defun reset-room-ids ()
  (setf *room-ids* 0))

(defun generate-test-player-db (num-players)
  "Generates NUM-PLAYERS instances of <player> and pushes them into *players*"
  (dotimes (i num-players)
    (pushnew (new-player) *players*))
  (format nil "Generated ~d players." num-players))

(defun generate-test-room-db (num-rooms)
  "Generates NUM-ROOMS instances of <room> and pushes them into *rooms*"
  (dotimes (i num-rooms)
    (pushnew (new-room) *rooms*))
  (format nil "Generated ~d rooms." num-rooms))

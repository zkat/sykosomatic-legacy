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
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Database ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;;;
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
(defun reset-player-ids ()
  (setf *player-ids* 0))

(defun reset-room-ids ()
  (setf *room-ids* 0))


(defun save-db (db filename)
  "Saves the provided DB into a file called FILENAME in the game's db/ dir."
  (cl-store:store db 
		  (ensure-directories-exist 
		   (merge-pathnames 
		    filename 
		    *db-directory*))))

(defun save-all-db ()
  "Saves all the database variables to file."
  (save-db *player-ids* "pid.db")
  (save-db *room-ids* "rid.db")
  (save-db *rooms* "rooms.db")
  (save-db *players* "players.db")
  (save-db *objects* "objects.db")
  (save-db *vocabulary* "vocabulary.db")
  (save-db *commands* "commands.db"))

(defun get-db-from-file (filename)
  "Gets the contents of FILENAME and returns them in a setf-able format."
  (cl-store:restore
   (merge-pathnames filename *db-directory*)))

(defun load-all-db ()
  "Loads everything from the db/ directory."
  (setf *player-ids*(get-db-from-file "pid.db"))
  (setf *room-ids*(get-db-from-file "rid.db"))
  (setf *rooms* (get-db-from-file "rooms.db"))
  (setf *players* (get-db-from-file "players.db"))
  (setf *objects* (get-db-from-file "objects.db"))
  (setf *vocabulary* (get-db-from-file "vocabulary.db"))
  (setf *commands* (get-db-from-file "commands.db")))

;;for testing
(defun generate-huge-room-db ()
  (dotimes (i 15000)
    (pushnew (make-instance '<room> :name (format nil "This is room #~a" i)) *rooms*)))

(defun generate-medium-room-db ()
  (dotimes (i 500)
    (pushnew (make-instance '<room> :name (format nil "This is room #~a" i)) *rooms*)))
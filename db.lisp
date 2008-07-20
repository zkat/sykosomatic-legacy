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
;;;========================================== Database ==========================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~ Variables ~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defvar *player-ids* 0)
(defvar *room-ids* 0)

(defvar *directions* '("north" "south" "east" ;;necessary? I don't think so.
		       "west" "northeast" "northwest"
		       "southeast" "southwest" "up"
		       "down" "enter"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~ Load/Save ~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defgeneric obj->file (obj path)
  (:documentation "Saves OBJECT to a file."))

(defun obj-list->files-in-dir (obj-list path)
  "Saves all OBJECTS in OBJECT-LIST into files within PATH"
  (loop for obj in obj-list
       do (obj->file obj path)))

(defun file->obj (filepath)
  (cl-store:restore filepath))

(defun files-in-path->obj-list (path)
  (let ((files (directory (merge-pathnames "*.*" path))))
    (loop for file in files
	 collect (cl-store:restore file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~ Utilities ~~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
(defun reset-player-ids ()
  (setf *player-ids* 0))

(defun reset-room-ids ()
  (setf *room-ids* 0))

(defun generate-test-players (num-players)
  (loop for i upto (1- num-players)
       collect (new-player)))

(defun generate-test-rooms (num-rooms)
  (loop for i upto (1- num-rooms)
       collect (new-room)))

;; Utility vars
;; ------------
(defvar *rooms* nil)
(defvar *players* nil)
(defvar *objects* nil)
(defvar *vocabulary* nil)
(defvar *commands* nil)
(defvar *current-player* nil)
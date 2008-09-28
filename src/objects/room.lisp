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

;; room.lisp
;;
;; Contains the <room> and <door> classes. Also holds functions that handle room generation from
;; file, saving/loading of rooms, setting of exits, getting of information about contents of room,
;; etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Room vars
;;;

(defvar *rooms* nil
  "List of available rooms. Rooms are also linked as a graph.")

(defvar *max-room-id* 0
  "Highest available room-id")

(defvar *room-id-lock* (bordeaux-threads:make-lock)
  "This lock should be held by anything that wants to manipulate *max-room-id*")

;;;
;;; Room-related classes
;;;

(defclass <room> (<game-object>)
  ((name
    :initform "NoNameRoom")
   (contents
    :initarg :contents
    :initform nil
    :accessor contents
    :documentation "All contents of this room, including entities")
   (room-id
    :initform (with-lock-held (*room-id-lock*) (incf *max-room-id*))
    :reader room-id
    :documentation "Universal room ID number"))
  (:documentation "Base class for rooms. This class adds a contents
 and room-id slot to a standard game object."))

(defclass <exit> (<game-object>)
  ((name
    :initform "exit")
   (open-p
    :initarg :open-p
    :initform t
    :accessor open-p
    :documentation "Is this exit open or closed?")
   (locked-p
    :initarg :locked-p
    :initform nil
    :accessor locked-p
    :documentation "Is the exit locked?")
   (next-room
    :initarg :next-room
    :initform nil
    :accessor next-room
    :documentation "Room object this exit points to"))
  (:documentation "A exit is something -- anything, that leads from one place
to another. In general, this can be an actual exit, but it can also be used as 
a mixin to make regular items (or even players) into portals and such."))

;;;
;;; Info
;;;

(defun room-p (obj)
  "Returns T if a given OBJ is an instance of <room>"
  (eq (class-name (class-of obj))
      '<room>))

(defun exit-p (obj)
  "Returns T if a given OBJ is an instance of <exit>."
  (eq (class-name (class-of obj))
      '<exit>))

(defun get-exits (room)
  "Returns a list of <exit> objects that the room contains."
  (let ((obj-list (append (contents room)
			  (features room))))
    (remove-if-not #'exit-p obj-list)))

;;;
;;; Room manipulation
;;;

;; Put something here that makes new exits.

;;;
;;; Load/Save
;;;

;;; Saving

(defmethod obj->file ((room <room>) path)
  (cl-store:store room (ensure-directories-exist
			(merge-pathnames
			 (format nil "room-~a.room" (room-id room))
			 path))))

(defun save-rooms ()
  "Saves all rooms in *rooms* to individual files in *rooms-directory*"
  (obj-list->files-in-dir *rooms* *rooms-directory*))

;;; Loading

(defun restore-max-room-id ()
  "Loads the highest room-id."
  (let ((room-ids (or (mapcar #'room-id *rooms*) '(0)))) ;reset to 0 if there are no rooms available.
    (with-lock-held (*room-id-lock*)
      (setf *max-room-id* 
	    (apply #'max room-ids)))))

;; NOTE: This breaks if tries to load an object that was created with an obsolete class.
(defun load-rooms ()
  "Loads saved rooms into the *rooms* list."
  (setf *rooms* (files-in-path->obj-list *rooms-directory* "room"))
  (restore-max-room-id))

;;;
;;; Testing
;;;

(defun new-test-room ()
  "Returns a new ROOM with its room-id in its name."
  (let ((room (make-instance '<room>)))
    (setf (name room) (format nil "Room #~a" (room-id room)))
    room))

(defun reset-max-room-id ()
  "Sets the highest room-id to 0."
  (with-lock-held (*room-id-lock*)
    (setf *max-room-id* 0)))

(defun generate-test-rooms (num-rooms)
  "Returns a LIST containing NUM-ROOMS generic instances of <room>."
  (loop
     for i upto (1- num-rooms)
     collect (make-instance '<room>)))

(defun make-rooms-from-file (file)
  "Generates rooms from a raw text FILE. Returns a list with all the generated rooms."
  (let ((rooms (with-open-file (in file)
		  (loop for line = (read in nil)
		     while line
		     collect line))))
    (mapcar #'eval rooms)))
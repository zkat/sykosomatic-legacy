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

;; room.lisp
;;
;; Contains the <room> and <door> classes. Also holds functions that handle room generation from
;; file, saving/loading of rooms, setting of exits, getting of information about contents of room
;; (like who the players are within the room), and getting the location of an <entity>
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

;;;
;;; Room-related classes
;;;

(defclass <room> (<game-object>)
  ((name
    :initform "A room without a name")
   (desc
    :initform "A room without a description"
    :documentation "A description of this room, in string format")
   (desc-long
    :initform "A room without a long description"
    :documentation "Long, detailed description of the room")
   (contents
    :initarg :contents
    :initform nil
    :accessor contents
    :documentation "All contents of this room, including entities")
   (room-id
    :initform (incf *max-room-id*)
    :reader room-id
    :documentation "Universal room ID number")
   (exits
    :initarg :exits
    :initform nil
    :accessor exits
    :documentation "Contains an assoc list of <exit> objects that refer to the next room.")))

(defun make-room (&key name desc desc-long features)
  "Simple constructor function for creating a room."
  (make-instance '<room> 
		 :name name :desc desc 
		 :desc-long desc-long :features features))

(defclass <door> (<game-object>)
  ((name
    :initform "A door")
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
    :documentation "Room object this exit points to")))

(defun make-door (&key name desc desc-long features next-room)
  (make-instance '<door> 
		 :name name :desc desc :desc-long desc-long 
		 :features features :next-room next-room))
;;;
;;; Room generation
;;;

;;There is a semi-dupe of this called new-player
(defun new-room ()
  "Returns a new ROOM after initializing the <ROOM> object"
  (let ((room (make-room)))
    (setf (name room)
	    (format nil "Room #~a" (room-id room)))
    room))

(defun make-rooms-from-file (file)
  "Generates a room from a raw text FILE."
  (let ((rooms (with-open-file (in file)
		  (loop for line = (read in nil)
		     while line
		     collect line))))
    (loop for room in rooms
	 collect (eval room))))

;;;
;;; Info
;;;

; functions that grab info specifically about a room go here.

;;;
;;; Room manipulation
;;;

(defun set-exit (from-room to-room direction)
  "Creates an EXIT that leads FROM-ROOM TO-ROOM in DIRECTION. NOT REFLEXIVE."
  (if (assoc direction (exits from-room) :test #'string-equal)
      ;; just change the room the existing exit goes to..
      (let ((door (cdr (assoc direction (exits from-room) :test #'string-equal))))
	(setf (next-room door) to-room))
      ;; else, make a brand-new exit.
      (let ((door (make-door :next-room to-room)))
	(pushnew (cons direction door) (exits from-room)))))

;; TODO - Although I don't think I can/should do this is a sane manner, because of how
;;        sykosomatic handles directional movement.
;;
;; (defun reflexive-set-exit (from-room to-room direction)
;;   "Manages exit creation. Mirrors the creation in the other room."
;;   nil)

(defgeneric put-entity (entity room)
  (:documentation "Changes where ENTITY is, taking care of any room-contents juggling."))

(defmethod put-entity ((entity <entity>) room) ; This should also make sure that the room where
  "Sets the LOCATION of ENTITY to ROOM."       ; <entity> currently resides has its CONTENTS
  (let ((old-room (location entity))
	(new-room room))
    (setf (location entity) new-room)
    (pushnew entity (contents new-room))
    (setf (contents old-room) (remove entity (contents old-room)))))

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
  (obj-list->files-in-dir *rooms* *rooms-directory*))

;;; Loading

(defun restore-max-room-id ()
  "Loads the highest room-id."
  (setf *max-room-id*
	(apply #'max
	       (mapcar #'room-id *rooms*))))

(defun load-rooms ()
  (setf *rooms* (files-in-path->obj-list *rooms-directory* "room")))

;;; Testing

(defun reset-room-ids ()
  (setf *room-ids* 0))

(defun generate-test-rooms (num-rooms)
  "Returns a LIST containing NUM-ROOMS instances of <room>."
  (loop
     for i upto (1- num-rooms)
     collect (new-room)))

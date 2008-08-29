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
    :initform (incf *room-ids*)
    :reader room-id
    :documentation "Universal room ID number")
   (exits
    :initarg :exits
    :initform nil
    :accessor exits
    :documentation "Contains an assoc list of <exit> objects that refer to the next room.")))

(defun make-room (&key name desc desc-long features)
  (make-instance '<room> :name name :desc desc :desc-long desc-long :features features))

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
    (with-accessors ((name name)) room
      (setf name (format nil "Room #~a" (room-id room))))
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

(defun whereis (entity) ;does this belong here?
  "Pretty-prints the NAME of the LOCATION of the ENTITY"
  (let ((loc (location entity)) (entity (name entity)))
    (format t "~a is in ~a" entity (name loc))
    loc))

(defun get-players (room) ;what about this?...
  "Fetches a list of players currently in ROOM."
  (with-accessors ((contents contents)) room
    (mapcar #'player-p contents)))

;;;
;;; Room manipulation
;;;

(defun set-exit (from-room to-room direction)
  "Creates an EXIT that leads FROM-ROOM TO-ROOM in DIRECTION. NOT REFLEXIVE."
  (if (not (assoc direction (exits from-room) :test #'string-equal))
      (let ((door (make-door :next-room to-room)))
	(pushnew (cons direction door) (exits from-room)))
      (let ((door (cdr (assoc direction (exits from-room) :test #'string-equal))))
	(setf (next-room door) to-room))))

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

(defmethod obj->file ((room <room>) path)
  (cl-store:store room (ensure-directories-exist
			(merge-pathnames
			 (format nil "room-~a.room" (room-id room))
			 path))))
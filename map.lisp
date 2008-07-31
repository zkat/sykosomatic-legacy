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
;;;========================================== Map classes =======================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;==================================== Map-related Functions ===================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~ Map Generation ~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;There is a semi-dupe of this called new-player
(defun new-room ()
  "Returns a new ROOM after initializing the <ROOM> object"
  (let ((room (make-instance '<room>)))
    (with-accessors ((name name)) room
      (setf name (format nil "Room #~a" (room-id room))))
    room))

(defmacro make-room (&key name desc desc-long)
  `(make-instance '<room> :name ,name :desc ,desc :desc-long ,desc-long))

(defun make-room-from-file (file) ;;uses jeebus' parser
  "Generates a room from a raw text FILE."
  (let ((rooms (with-open-file (in file)
		  (loop for line = (read in nil)
		     while line
		     collect line))))
    (loop for room in rooms
	 collect (eval room))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~ Info ~~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun whereis (entity)
  "Pretty-prints the NAME of the LOCATION of the ENTITY"
  (let ((loc (location entity)) (entity (name entity)))
    (format t "~a is in ~a" entity (name loc))
    loc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~ Manipulation ~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun set-exit (from-room to-room direction) ;seems to be working fine... we'll see.
  "Receives a DIRECTION symbol. Gets the <door> object in the EXITS hash of
FROM-ROOM and sets it to TO-ROOM, and adds the"
  (if (not (assoc direction (exits from-room) :test #'string-equal))
      (let ((door (make-instance '<door> :next-room to-room)))
	(pushnew (cons direction door) (exits from-room)))
      (let ((door (cdr (assoc direction (exits from-room) :test #'string-equal))))
	(setf (next-room door) to-room))))

(defgeneric put-entity (entity room)
  (:documentation "Changes where ENTITY is, taking care of any room-contents juggling."))

(defmethod put-entity ((entity <entity>) room) ; This should also make sure that the room where
  "Sets the LOCATION of ENTITY to ROOM."       ; <entity> currently resides has its CONTENTS
  (let ((old-room (location entity))
	(new-room room))
    (setf (location entity) new-room)
    (pushnew entity (contents new-room))
    (setf (contents old-room) (remove entity (contents old-room)))))

(defgeneric move (entity direction)
  (:documentation "Moves ENTITY in DIRECTION"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~ Load/Save ~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defmethod obj->file ((room <room>) path)
  (cl-store:store room (ensure-directories-exist
			(merge-pathnames
			 (format nil "room-~a.room" (room-id room))
			 path))))
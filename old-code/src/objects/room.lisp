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
;; Contains the <room> and <door> classes. Holds functions that handle
;; setting of exits, getting of information about contents of room, etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic.object)

;;;
;;; Room-related classes
;;;

(define-persistent-class <room> (<game-object>)
  ((name
    :update
    :initform "NoNameRoom"
    :index-type hash-index
    :index-initargs (:test #'equalp)
    :index-reader rooms-with-name
    :index-values all-rooms)   
   (contents
    :update
    :initarg :contents
    :initform nil
    :accessor contents
    :documentation "All contents of this room, including entities"))
  (:documentation "Base class for rooms. This class adds a contents
 and room-id slot to a standard game object."))

(define-persistent-class <exit> (<game-object>)
  ((name
    :update
    :initform "exit"
    :index-type hash-index
    :index-initargs (:test #'equalp)
    :index-reader exits-with-name
    :index-values all-exits)
   (open-p
    :update
    :initarg :open-p
    :initform t
    :accessor open-p
    :documentation "Is this exit open or closed?")
   (locked-p
    :update
    :initarg :locked-p
    :initform nil
    :accessor locked-p
    :documentation "Is the exit locked?")
   (next-room
    :update
    :initarg :next-room
    :initform nil
    :accessor next-room
    :index-type hash-index
    :index-reader exits-that-lead-to
    :documentation "Room object this exit points to"))
  (:documentation "A exit is something -- anything, that leads from one place
to another. In general, this can be an actual exit, but it can also be used as 
a mixin to make regular items (or even avatars) into portals and such."))

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
(defgeneric remove-object-from-room (object)
  (:documentation "Removes a target object from a room."))

(defgeneric put-object-in-room (object room)
  (:documentation "Puts the given object into the contents of a room."))

(defmethod put-object-in-room ((object <game-object>) room)
  (with-transaction ()
    (setf (location object) room)
    (pushnew object (contents room) :test #'equal)))

(defun write-to-others-in-room (caller format-string &rest format-args)
  "Works like FORMAT, writing its arguments to everyone in CALLER's location, except to CALLER."
  (when (location caller)
    (let ((other-avatars (remove caller (get-avatars (location caller)))))
     (loop for avatar in other-avatars
	do (write-to-target avatar format-string format-args)))))

(defun write-to-room (room format-string &rest format-args)
  (let ((avatars (get-avatars room)))
    (loop for avatar in avatars
	 do (apply #'write-to-target avatar format-string format-args))))

;; Put something here that makes new exits.


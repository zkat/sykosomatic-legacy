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

;; avatar.lisp
;;
;; Holds the <avatar> class. Some management functions exist, too, but those could be moved out.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Avatar class
;;;

(define-persistent-class <avatar> (<mobile>)
  ((name
    :update
    :initform "NoNameAvatar"
    :index-type string-unique-index
    :index-initargs (:test #'equalp)
    :index-reader avatar-with-name
    :index-values all-avatars)
   (last-location
    :update
    :initform nil
    :accessor last-location
    :documentation "Last place where this avatar was before disconnection.")
   (client
    :update
    :transient t
    :initform nil
    :accessor client
    :documentation "Client currently associated with this character.")
   (account
    :update
    :initarg :account
    :initform nil
    :accessor account
    :documentation "Account this <avatar> object belongs to."))
  (:documentation "Standard AVATAR for sykosomatic. Simply adds enough to make it different from
a mobile. This is what avatars will inhabit."))

;;;
;;; Info
;;;

(defun avatar-p (obj)
  "Returns T if a given OBJ is an instance of <AVATAR>."
  (eq (class-name (class-of obj))
      '<avatar>))

(defun get-avatars (room)
  "Fetches a list of avatars currently in ROOM."
  (remove-if-not #'avatar-p (contents room)))

;;;
;;; Avatar functions
;;;
(defmethod remove-object-from-room ((avatar <avatar>))
  "Removes avatar from its current location"
  (let ((room (location avatar)))
    (with-transaction ()
     (setf (last-location avatar) room)
     (setf (location avatar) nil)
     (when room
       (setf (contents room) (remove avatar room))))))

(defmethod write-to-target ((avatar <avatar>) format-string &rest format-args)
  "Sends output to a avatar."
  (let ((avatar-client (client avatar)))
    (if avatar-client
	(apply #'write-to-client avatar-client format-string format-args)
	(error "Avatar is not connected."))))

(defun initialize-avatar (avatar)
  (if (last-location avatar)
      (put-object-in-room avatar (last-location avatar))
      (put-object-in-room avatar *newbie-area*)))

(defun disconnect-avatar (avatar)
  "Disconnects the given avatar from the game."
  (write-to-others-in-room avatar "OOC - ~a has disconnected" (name avatar))
  (remove-object-from-room avatar)
  (when (client avatar)
    (disconnect-client (client avatar))))

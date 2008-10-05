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
;; Holds the <avatar> class. Also contains some functions for character loading/saving.
;; Some management functions exist, too, but those could be moved out.
;;
;; TODO: Rename all mentions of <avatar> and avatar to avatar
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

(defmethod write-to-target ((avatar <avatar>) format-string &rest format-args)
  "Sends output to a avatar."
  (let ((avatar-client (client avatar)))
    (if avatar-client
	(apply #'write-to-client avatar-client format-string format-args)
	(error "Avatar is not connected."))))

(defun disconnect-avatar (avatar)
  "Disconnects the given avatar from the game."
  (disconnect-client (client avatar))
  (setf (client avatar) nil))


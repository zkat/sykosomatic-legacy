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

;; player.lisp
;;
;; Holds the <player> class. Also contains some functions for character loading/saving.
;; Some management functions exist, too, but those could be moved out.
;;
;; TODO: Rename all mentions of <player> and player to avatar
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Player class
;;;

(define-persistent-class <player> (<mobile>)
  ((name
    :update
    :initform "NoNamePlayer"
    :index-type string-unique-index
    :index-reader player-with-name
    :index-values all-players)
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
    :documentation "Account this <player> object belongs to."))
  (:documentation "Standard PLAYER for sykosomatic. Simply adds enough to make it different from
a mobile. This is what players will inhabit."))

;;;
;;; Info
;;;

(defun player-p (obj)
  "Returns T if a given OBJ is an instance of <PLAYER>."
  (eq (class-name (class-of obj))
      '<player>))

(defun get-players (room)
  "Fetches a list of players currently in ROOM."
  (remove-if-not #'player-p (contents room)))

;;;
;;; Player functions
;;;

(defmethod write-to-target ((player <player>) format-string &rest format-args)
  "Sends output to a player."
  (let ((player-client (client player)))
    (if player-client
	(apply #'write-to-client player-client format-string format-args)
	(error "Player is not connected."))))

(defun disconnect-player (player)
  "Disconnects the given player from the game."
  (disconnect-client (client player))
  (setf (client player) nil))


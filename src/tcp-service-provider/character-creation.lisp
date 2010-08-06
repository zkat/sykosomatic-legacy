;; Copyright 2008-2010 Kat Marchán

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

;; character-creation.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defclass body ()
  ((name :accessor name :initarg :name)
   (description :accessor description :initarg :description)))

(defun/cc choose-character (client)
  (setf (body (soul client))
        (make-instance 'body
                       :name (prompt-client client "~&Pick a name for your character: ")
                       :description (prompt-client client "~&Describe your character: ")))
  (format client "~&You are now logged in as ~A.~%" (name (body (soul client))))
  (format client "~&Commands: 'look' and 'quit'. Type anything else to chat.~%")
  (broadcast-to-room client "~&~A enters the world.~%" (name (body (soul client))))
  client)

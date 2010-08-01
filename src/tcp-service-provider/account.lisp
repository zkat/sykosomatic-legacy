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

;; account.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defvar *accounts* nil
  "List of existing accounts.")

;;;
;;; Account class
;;;
(defclass account ()
  ((username :accessor username :initarg :username)
   (password-hash :accessor password-hash)
   (email :accessor email :initarg :email)))

(defmethod initialize-instance :after ((account account) &key password)
  (setf (password-hash account) (hash-password password))
  (push account *accounts*))

;;;
;;; Account utils
;;;
(defun find-account (username)
  (find username *accounts* :key #'username :test #'string=))

(defun account-exists-p (username)
  (find-account username))

(defun confirm-password (account input)
  (when (string= (password-hash account)
                 (hash-password input))
    account))

;;;
;;; Login
;;;
(defun/cc maybe-login (client)
  (let* ((username (prompt-client client "~&Username: "))
         (account (find-account username)))
    (if account
        (login client account)
        (no-such-account client username))))

(defun/cc login (client account)
  (let ((account (confirm-password account (prompt-client client "~&Password: "))))
    (if account
        (progn
          (setf (soul client) (make-instance 'soul :account account :client client))
          (format client "~&You are now logged in as ~A.~%" (username (account (soul client))))
          (format client "~&Commands: 'look' and 'quit'. Type anything else to chat.~%")
          (broadcast-to-room client "~&~A enters the world.~%" (username (account (soul client)))))
        (progn
          (format client "~&Wrong password.~%")
          (maybe-login client)))))

(defun/cc no-such-account (client username)
  (if (client-y-or-n-p client "~&No such account. Create one? (Y/n) ")
      (create-account client username)
      (progn
        #+nil(format client "~&Bye!~%")
        (close client))))

;;;
;;; Account creation
;;;
(defun/cc create-account (client username)
  (let ((username (pick-username client username))
        (password (pick-password client))
        (email (pick-email client)))
    (make-instance 'account :username username :password password :email email)
    (format client "~&Account successfully created~%")
    (maybe-login client)))

(defun/cc pick-username (client username)
  (if (client-y-or-n-p client "~&Use ~A as your username? (Y/n) " username)
      username
      (pick-username client (prompt-client client "~&Pick a new username: "))))

(defun/cc pick-password (client)
  (let ((password (prompt-client client "~&Pick a password: "))
        (confirm (prompt-client client "~&Confirm your password: ")))
    (if (string= password confirm)
        password
        (progn
          (format client "~&Passwords did not match.~%")
          (maybe-login client)))))

(defun/cc pick-email (client)
  (prompt-client client "~&Please enter your email address: "))

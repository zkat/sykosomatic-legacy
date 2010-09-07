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

;;;
;;; Login
;;;
(defun/cc maybe-login (client)
  (let ((username (prompt-client client "~&Username: ")))
    (aif (find-account username)
         (login client it)
         (no-such-account client username))))

(defun/cc login (client account)
  (aif (confirm-password account (prompt-client client "~&Password: "))
       (progn
         (setf (soul client) (make-instance 'soul :account it :client client))
         (choose-character client))
       (progn
         (format client "~&Wrong password.~%")
         (maybe-login client))))

(defun/cc no-such-account (client username)
  (if (client-y-or-n-p client "~&No such account. Create one? (Y/n) ")
      (create-account client username)
      (progn
        #+nil(format client "~&Bye!~%")
        (disconnect client :close))))

;;;
;;; Account creation
;;;
(defun/cc create-account (client username)
  (let ((username (pick-username client username))
        (password (pick-password client))
        (email (pick-email client)))
    (make-account username password email)
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

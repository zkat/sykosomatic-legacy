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

;; login.lisp
;;
;; Functions and other tools that deal with logging in and out.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :org.sykosomatic.core)

;;;
;;; Account Login
;;;

(defun/cc login-menu (client)
  "Top-level menu for client login."
  (write-to-client client "~&Welcome to SykoSoMaTIC!~%")
  (write-to-client client "Take your pick: ~%")
  (write-to-client client "------------------------~%")
  (write-to-client client "[L]og in to your account.~%")
  (write-to-client client "[C]reate a new account.~%")
  (write-to-client client "[Q]uit~%")
  (write-to-client client "-------------------------~%")
  (let ((choice (prompt-client client "> ")))
    (cond ((string-equal choice "L")
	   (login-client client))
	  ((string-equal choice "C")
	   (create-new-account client))
	  ((string-equal choice "Q")
	   (disconnect-client client))
	  (t
	   (write-to-client client "~&Invalid choice. Try again.")))))

(defun/cc account-menu (client)
  "Simple selection menu that new clients once they've logged into an account."
  (write-to-client client "~&Choose your destiny: ~%")
  (write-to-client client "-----------------------~%")
  (write-to-client client "1. Choose a character~%")
  (write-to-client client "-----------------------~%~%")
  (let ((choice (prompt-client client "Your choice: ")))
    (cond ((equal choice "1")
	   (choose-avatar client))
	  (t
	   (write-to-client client "~&Invalid choice.")
	   (account-menu client)))))

(defun/cc login-client (client)
  "Logs a user into their account"
  (let ((account (validate-login client (prompt-username client))))
    (when account
      ;; active clients?
      (register-client-ip-with-account (ip client) account)
      (account-menu client))))

(defun/cc validate-login (client account)
  "Prompts the user for a password, and validates the login."
  (let ((password (prompt-client client "~&Password: ")))
    (if (equal (hash-password password) (password account))
	account
	(validate-login client account))))
(defun/cc prompt-username (client)

  "Prompts a client for a username, returns a valid account."
  (let* ((account-name (prompt-client client "~%Username: "))
	 (account (account-with-name account-name)))
    (if account
	account
	(progn
	  (write-to-client client "~&Invalid username, please try again.")
	  (prompt-username client)))))

;;;
;;; Utilities
;;;

(defun print-available-avatars (client)
  "Prints a list of available avatars."
  (let ((avatars (avatars (account client))))
    (write-to-client client "~%Characters:~%-----------~%~%")
    (loop for avatar in avatars
       do (write-to-client client "~&~a~&" (name avatar)))))

(defun register-client-ip-with-account (ip account)
  (with-transaction ()
    (pushnew ip (known-ips account))))
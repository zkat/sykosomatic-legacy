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
;;;=========================================== Accounts =========================================;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;==================== Class ===================;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defclass <account> ()
  ((username
    :initarg :username
    :reader username)
   (password
    :initarg :password
    :accessor password)
   (id
    :initform (incf *account-ids*)
    :reader id
    :documentation "Unique account ID number.")
   (avatars
    :accessor avatars
    :initform nil
    :documentation "Characters belonging to this account.")
   (client
    :accessor client
    :initform nil
    :documentation "Client currently associated with this account.")
   (account-type
    :accessor account-type
    :initarg :account-type
    :initform nil
    :documentation "The type of account. Used to determine access levels.")
   (known-ips
    :initarg :known-ips
    :accessor know-ips
    :initform nil
    :documentation "All IPs this account has been known no use.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~~~~~ Functions ~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;        Login        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;
(defun login-to-account (client)
  "Logs a user into their account"
  (write-to-client "Welcome to SykoSoMaTIC Beta(tm)."))

;; TODO
(defun start-client ()
  (let ((username (prompt-read "Username: ")))
    (if (player-exists? username)
        (if (prompt-password (lookup-password username))
	    username
	    (error (format nil "Invalid authentication for user ~a." username)))
        (when (y-or-n-p "You wish to be known as ~a?" username)
          (make-new-player username)))))

;; TODO
(defun prompt-password (correct-password)
  (let ((password (prompt-read "Password")))
    (if (equal (hash-password password) correct-password)
	t)))
;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Account Creation  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; TODO
(defun make-new-player (username)
  (format *query-io* "Welcome to BMUD new player.~%")
  (format *query-io* "I'm going to need to ask you some questions to make your account.~%")
  (multiple-value-bind (firstname lastname) (setup-name)
    (let ((email (setup-email))
	  (username (setup-username username))
	  (password (setup-password)))
      (values username password firstname lastname email))))
;; TODO
(defun setup-name ()
  "blegh. Cleaning up shittier code than mine :-\ "
  (let ((firstname (prompt-read "Please enter your first name"))
	(lastname (prompt-read "Please enter your first name")))
    (if (y-or-n-p "Greetings ~a ~a. Is this name correct" firstname lastname)
	(values firstname lastname)
	(setup-name))))
;; TODO
(defun setup-email ()
  "cleaned-up e-mail setup without the suck."
  (let ((email (prompt-read "Please enter your email address")))
    (if (cl-ppcre:scan "^[\\w._%\\-]+@[\\w.\\-]+\\.([A-Za-z]{2}|com|edu|org|net|biz|info|name|aero|biz|info|jobs|museum|name)$" email)
	(if (y-or-n-p "Is the email address ~a correct?" email)
	    email
	    (setup-email))
	(progn
	  (format *query-io* "I'm sorry, ~a is not a valid email address.~%" email)	  
	  (setup-email)))))
;; TODO
(defun setup-username (username)
  "Setup the user's username."
  (format *query-io* "It seems that you chose username ~a.~%" username)
  (if (y-or-n-p "Would you like to use this username?")
      username
      (let ((username (prompt-read "Please enter your desired username")))
	(setup-username username))))
;; TODO
(defun setup-password ()
  "Allow the user to choose a password."
  (let* ((password (prompt-read "Please choose a password"))
	 (pass-confirm (prompt-read "Please retype the password")))
    (if (equal password pass-confirm)
	password
	(progn
	  (format *query-io* "~%Passwords did not match, trying again.~%")
	  (setup-password)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Account Management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;


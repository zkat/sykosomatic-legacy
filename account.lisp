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

;; account.lisp
;;
;; This file contains the <account> class, meant to hold some basic information about user accounts 
;; like login/pass, characters available, and the client currently connected to the account. This 
;; file also contains the functions that handle user login, account creation, and account management.
;;
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
(defun get-account-by-name (username)
  "Fetches an account using a username."
  (find username *accounts* :key #'string-equal))

;;Note: (if .. (progn ..)) should be replaced by (when .. forms*)
(defun login-client (client)
  "Logs a user into their account"
  (let ((account (validate-login client (prompt-username client))))
    (if account
	(progn 
	  (setf (account client) account)
	  (setf (client account) client)
	  (pushnew (ip client) (know-ips account))
	  (account-menu client)))))

(defun prompt-username (client)
  "Prompts a client for a username, returns a valid account."
  (let* ((account-name (prompt-client client "~%Username: "))
	 (account (get-account-by-name account-name)))
    (if account
	account
	(progn
	  (write-to-client client "~&Invalid username, please try again.")
	  (prompt-username client)))))

(defun validate-login (client account)
  "Prompts the user for a password, and validates the login."
  (let ((password (prompt-client client "~&Password: ")))
    (if (equal password (password account))
	account
	(validate-login client account))))

(defun account-menu (client)
  "Simple selection menu that new clients once they've logged into an account."
  (write-to-client client "~&Choose your destiny: ~%")
  (write-to-client client "-----------------------~%")
  (write-to-client client "1. Create a new character~%")
  (write-to-client client "2. Enter existing character~%")
  (write-to-client client "-----------------------~%~%")
  (let ((choice (prompt-client client "Your choice: ")))
    (cond ((string-equal choice "1")
	   (create-an-avatar client))
	  ((string-equal choice "2")
	   (choose-avatar client))
	  (t
	   (progn
	     (write-to-client client "~&Invalid choice.")
	     (account-menu client))))))

(defun create-an-avatar (client)
  "Takes user through the avatar-creation process."
  (let ((account (account client)))
    (let* ((avatar-name (prompt-client client "~&Choose a name for your character: "))
	   (avatar (make-player 
		    :name avatar-name
		    :desc "generic description" 
		    :desc-long "generic long-description")))
      (pushnew avatar *players*)
      (pushnew avatar (avatars account)))))

(defun choose-avatar (client)
  "Lets a player choose an existing avatar to play on."
  (print-available-avatars client)
  (let* ((avatars (avatars (account client)))
	 (avatar-choice (prompt-client client "~%~%Choose your destiny: "))
	 (avatar (find avatar-choice avatars :test #'string-equal)))
    (if avatar
	(progn
	  (setf (avatar client) avatar)
	  (player-main-loop client))
	(progn
	  (write-to-client client "~&No such character, please try again.~%")
	  (choose-avatar client)))))

(defun print-available-avatars (client)
  "Prints a list of available avatars."
  (let ((avatars (avatars (account client))))
    (write-to-client client "~%Characters:~%-----------~%~%")
    (loop for avatar in avatars
       do (write-to-client client "~&~a~&" (name avatar)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Account Creation  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; TODO
(defun setup-account (username)
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


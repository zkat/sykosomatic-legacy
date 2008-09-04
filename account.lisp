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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Account vars
;;;

(defvar *accounts* (make-hash-table :test #'equalp)
  "Hash table holding all existing accounts.")

(defvar *max-account-id* 0)

;;;
;;; Account class
;;;
(defclass <account> ()
  ((username
    :initarg :username
    :reader username)
   (password
    :initarg :password
    :accessor password)
   (email
    :initarg :email
    :accessor email)
   (id
    :initform (incf *max-account-id*)
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

(defun make-account (&key username password email)
  "Generic constructor"
  (make-instance '<account> :username username :password password :email email))

;;;
;;; Account Login
;;;

(defun/cc account-menu (client)
  "Simple selection menu that new clients once they've logged into an account."
  (write-to-client client "~&Choose your destiny: ~%")
  (write-to-client client "-----------------------~%")
  (write-to-client client "1. Create a new character~%")
  (write-to-client client "2. Enter existing character~%")
  (write-to-client client "-----------------------~%~%")
  (let ((choice (prompt-client client "Your choice: ")))
    (cond ((equal choice "1")
	   (create-an-avatar client))
	  ((equal choice "2")
	   (choose-avatar client))
	  (t
	   (write-to-client client "~&Invalid choice.")
	   (account-menu client)))))

(defun get-account-by-name (username)
  "Fetches an account using a username."
  (gethash username *accounts*))

(defun/cc login-client (client)
  "Logs a user into their account"
  (let ((account (validate-login client (prompt-username client))))
    (when account
      (setf (account client) account)
      (setf (client account) client)
      (pushnew (ip client) (know-ips account))
      (account-menu client))))

(defun/cc prompt-username (client)
  "Prompts a client for a username, returns a valid account."
  (let* ((account-name (prompt-client client "~%Username: "))
	 (account (get-account-by-name account-name)))
    (if account
	account
	(progn
	  (write-to-client client "~&Invalid username, please try again.")
	  (prompt-username client)))))
 
(defun/cc validate-login (client account)
  "Prompts the user for a password, and validates the login."
  (let ((password (prompt-client client "~&Password: ")))
    (if (equal (hash-password password) (password account))
	account
	(validate-login client account))))

(defun print-available-avatars (client)
  "Prints a list of available avatars."
  (let ((avatars (avatars (account client))))
    (write-to-client client "~%Characters:~%-----------~%~%")
    (loop for avatar in avatars
       do (write-to-client client "~&~a~&" (name avatar)))))

;; TODO - player-main-loop thing is borked.
;; (defun choose-avatar (client)
;;   "Lets a player choose an existing avatar to play on."
;;   (print-available-avatars client)
;;   (let* ((avatars (avatars (account client)))
;; 	 (avatar-choice (prompt-client client "~%~%Choose your destiny: "))
;; 	 (avatar (find avatar-choice avatars :test #'string-equal)))
;;     (if avatar
;; 	(progn
;; 	  (setf (avatar client) avatar)
;; 	  (player-main-loop client))
;; 	(progn
;; 	  (write-to-client client "~&No such character, please try again.~%")
;; 	  (choose-avatar client)))))

;;;
;;; Account Creation
;;;

(defun/cc create-new-account (client)
  "Creates a new account and adds it to available accounts."
  (let ((account (setup-account client)))
    (unless (gethash (username account) *accounts*)
      (setf (gethash (username account) *accounts*) account))))

(defun/cc setup-account (client)
  "Sets up user account."
  (write-to-client client "Alright, let's set up your account...~%")
  (let* ((username (setup-username client))
	 (password (setup-password client))
	 (email (setup-email client)))
    (make-account :username username :password password :email email)))

(defun/cc setup-username (client)
  "Prompts client for a username."
  (let ((username (prompt-client client "Please enter your desired username: ")))
    (if (confirm-username-sanity username)
	(progn
	  (write-to-client client "~%It seems that you chose username ~a.~%" username)
	  (if (client-y-or-n-p client "Would you like to use this username? ")
	      username
	      (setup-username client)))
	(progn
	  (write-to-client client "Username too long (must be under 16 chars)~%")
	  (setup-username client)))))

(defun/cc setup-password (client)
  "Prompts client for a password."
  (let ((password (prompt-client client "~%Choose a password: "))
	(pass-confirm (prompt-client client "Retype your password: ")))
    (if (and (equal password pass-confirm)
	     (confirm-password-sanity password))
	(hash-password password)
	(progn
	  (write-to-client client "~&Passwords did not match, try again.~%")
	  (setup-password client)))))

(defun/cc setup-email (client)
  "Prompts client for a correct e-mail address."
  (let ((email (prompt-client client "~%Please enter your email address: ")))
    (if (confirm-email-sanity email)
	(progn
	  (write-to-client "~%You chose ~a as your email address.~%" email)
	  (if (client-y-or-n-p client "Is this email address correct?")
	     email
	     (setup-email client)))
	(progn
	  (write-to-client client "I'm sorry, ~a is not a valid email address.~%" email)	  
	  (setup-email client)))))

;; NOTE: This isn't used (yet)
(defun/cc setup-name (client)
  "Prompts client for first and last name."
  (let* ((firstname (prompt-client client "Please enter your first name"))
	 (lastname (prompt-client client "Please enter your first name")))
    (if (client-y-or-n-p client "Greetings ~a ~a. Is this name correct" firstname lastname)
	(values firstname lastname)
	(setup-name client))))

;;;
;;; Account Management
;;;

;;NIL


;;;
;;; Load/Save
;;;

;;; Save
(defun save-accounts ()
  "Saves the account database into *account*"
  (cl-store:store *accounts* (ensure-directories-exist
			      (merge-pathnames
			       "accounts.sy"
			       *game-directory*))))

;;; Load
(defun load-accounts ()
  "Loads the account database into *accounts*"
  (setf *accounts* (cl-store:restore (ensure-directories-exist 
				      (merge-pathnames
				       "accounts.sy"
				       *game-directory*)))))
(defun restore-max-account-id ()
  "Loads the highest account id"
  (let ((account-ids (or (loop for account being each hash-value of *accounts*
			 collect (id account))
			 '(0))))
    (apply #'max account-ids)))

;;;
;;; Utils
;;;

(defun hash-password (password)
  "Password hashing function."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
     :sha256
     (ironclad:ascii-string-to-byte-array
      password))))

(defun confirm-username-sanity (username)
  "Confirms username sanity. Usernames have to be between 6 and 16 chars long, and may only
be composed of alphanumeric characters."
  (and (>= (length username) 6)
       (<= (length username) 16)
       (not (find-if-not #'alphanumericp username))))

(defun confirm-password-sanity (password)
  "Confirms password is acceptable. Password needs to be 8 to 32 chars long, and may only contain
a set of characters defined as CL's standard-char type."
  (and (>= (length password) 8)
       (<= (length password) 32)
       (not (find-if-not #'standard-char-p password))))

(defun confirm-email-sanity (email)
  "Checks that EMAIL is a legal e-mail address. Only accepts certain domains."
  (cl-ppcre:scan 
   "^[\\w._%\\-]+@[\\w.\\-]+\\.([A-Za-z]{2}|com|edu|org|net|biz|info|name|aero|biz|info|jobs|museum|name)$" 
   email))



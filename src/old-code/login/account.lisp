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

;; account.lisp
;;
;; This file contains the <account> class, meant to hold some basic information about user accounts
;; like login/pass, characters available, and the client currently connected to the account. This
;; file also contains the functions that handle user login, account creation, and account management.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic.core)

;;;
;;; Account class
;;;
(define-persistent-class <account> ()
  ((username
    :update
    :initarg :username
    :reader username
    :index-type string-unique-index
    :index-reader account-with-name
    :index-values all-accounts)
   (password
    :update
    :initarg :password
    :accessor password)
   (email
    :update
    :initarg :email
    :accessor email
    :index-type string-unique-index
    :index-reader account-with-email)
   (avatars
    :update
    :accessor avatars
    :initform nil
    :documentation "Characters belonging to this account.")
   (current-clients
    :update
    :transient t
    :initform nil
    :accessor clients
    :documentation "Clients currently associated with this account.")
   (account-type
    :update
    :accessor account-type
    :initarg :account-type
    :initform :basic
    :index-type hash-index
    :index-reader accounts-with-type
    :documentation "The type of account. Used to determine access levels.")
   (known-ips
    :update
    :initarg :known-ips
    :accessor known-ips
    :initform nil
    :documentation "All IPs this account has been known no use.")))

;;;
;;; Account Creation
;;;

(defun/cc create-new-account (client)
  "Sets up user account."
  (write-to-client client "Alright, let's set up your account...~%")
  (let* ((username (setup-username client))
         (password (setup-password client))
         (email (setup-email client)))
    (make-instance '<account> :username username :password password :email email)))

(defun/cc setup-username (client)
  "Prompts client for a username."
  (let ((username (prompt-client client "Please enter your desired username: ")))
    (if (confirm-username-sanity username)
        (progn
          (write-to-client client "~%It seems that you chose username ~a.~%" username)
          (if (client-y-or-n-p client "Would you like to use this username? ")
              (if (user-exists-p username)
                  (progn
                    (write-to-client client "~&An account with that username already exists.~%")
                    (setup-username client))
                  username)
              (setup-username client)))
        (progn
          (write-to-client client "~&Username must between 4 and 16 characters long and contain only letters and numbers.~%~%")
          (setup-username client)))))

(defun/cc setup-password (client)
  "Prompts client for a password."
  (let ((password (prompt-client client "~%Choose a password: "))
        (pass-confirm (prompt-client client "~&Retype your password: ")))

    (if (not (equal password pass-confirm))
        (progn
          (write-to-client client "~&Passwords did not match, try again.~%")
          (setup-password client))
        (if (confirm-password-sanity password)
            (hash-password password)
            (progn
              (write-to-client client "~&Password must be between 6 and 32 characters in length.~%~%")
              (setup-password client))))))

(defun/cc setup-email (client)
  "Prompts client for a correct e-mail address."
  (let ((email (prompt-client client "~%Please enter your email address: ")))
    (if (confirm-email-sanity email)
        (progn
          (write-to-client client  "~%You chose ~a as your email address.~%" email)
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
;;; Utils
;;;

(defun user-exists-p (username)
  "Checks if the username is already being used"
  (account-with-name username))

;; This is still probably not very safe. That, or the way I'm handling it might be wrong.
(defun hash-password (password)
  "Password hashing function."
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array
     password))))


;;; Sanity checkers

(defun confirm-username-sanity (username)
  "Confirms username sanity. Usernames have to be between 6 and 16 chars long, and may only
be composed of alphanumeric characters."
  (and (>= (length username) 4)
       (<= (length username) 16)
       (not (find-if-not #'alphanumericp username))))

(defun confirm-password-sanity (password)
  "Confirms password is acceptable. Password needs to be 8 to 32 chars long, and may only contain
a set of characters defined as CL's standard-char type."
  (and (>= (length password) 6)
       (<= (length password) 32)
       (not (find-if-not #'standard-char-p password))))

(defun confirm-email-sanity (email)
  "Checks that EMAIL is a legal e-mail address. Only accepts certain domains."
  (cl-ppcre:scan
   "^[\\w._%\\-]+@[\\w.\\-]+\\.([A-Za-z]{2}|com|edu|org|net|biz|info|name|aero|biz|info|jobs|museum|name)$"
   email))

;;; Conditions
(define-condition account-creation-error (error)
  ((text :initarg :text :reader text))
  (:documentation "Signaled when some condition happens during account creation."))

(define-condition account-exists-error (account-creation-error)
  ((text :initarg :text :reader text))
  (:documentation "Signaled if an account with this data already exists."))


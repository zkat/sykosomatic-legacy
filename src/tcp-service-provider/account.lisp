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

(declaim (optimize debug))

(defparameter *db* (ensure-db "sykosomatic"))

(defun gen-uuids (&optional (count 1))
  (hashget (get-uuids (server *db*) :number count) "uuids"))
(defun gen-uuid ()
  (car (gen-uuids 1)))

(defclass account ()
  ((%document :initarg :document)))

(defun find-account-document (username)
  (let* ((response (get-document *db* "_design/accounts/_view/by_username" :key username))
         (rows (hashget response "rows")))
    (when rows
      (get-document *db* (hashget (car rows) "id")))))

(defun find-account (username)
  (let ((doc (find-account-document username)))
    (when doc (make-instance 'account :document doc))))

(defun make-account (username password email)
  (when (find-account username)
    (error "That username is already being used."))
  (let ((uuid (gen-uuid)))
    (put-document *db* uuid
                  (mkhash "username" username
                          "password" (hash-password password)
                          "email" email))
    (make-instance 'account :document (get-document *db* uuid))))

(defmethod update ((account account))
  (let ((old-doc (slot-value account '%document)))
    (setf (slot-value account '%document)
          (get-document *db* (hashget old-doc "_id")))
    account))

(defgeneric save (obj)
  (:method ((account account))
    (let ((doc (slot-value account '%document)))
      (put-document *db* (hashget doc "_id") doc))
    account))

(defgeneric username (account)
  (:method ((account account))
    (hashget (slot-value account '%document) "username")))
(defgeneric (setf username) (new-value account)
  (:method (new-value (account account))
    (setf (hashget (slot-value account '%document) "username") new-value)))
(defgeneric password (account)
  (:method ((account account))
    (hashget (slot-value account '%document) "password")))
(defgeneric (setf password) (new-value account)
  (:method (new-value (account account))
    (setf (hashget (slot-value account '%document) "password") (hash-password new-value))))
(defgeneric email (account)
  (:method ((account account))
    (hashget (slot-value account '%document) "email")))
(defgeneric (setf email) (new-value account)
  (:method (new-value (account account))
    (setf (hashget (slot-value account '%document) "email") new-value)))
(defgeneric uuid (obj)
  (:method ((account account))
    (hashget (slot-value account '%document) "_id")))
(defgeneric revision (account)
  (:method ((account account))
    (hashget (slot-value account '%document) "_rev")))

(defun ensure-account-design-doc ()
  (or (handler-case
          (get-document *db* "_design/accounts")
        (document-not-found () nil))
      (put-document *db* "_design/accounts"
                    (mkhash "language" "common-lisp"
                            "views" (mkhash "by_username"
                                            (mkhash "map"
                                                    (prin1-to-string
                                                     '(lambda (doc &aux (username
                                                                         (hashget doc "username")))
                                                       (when username
                                                         (emit username (hashget doc "_id")))))))))))

;;;
;;; Account utils
;;;
(defun account-exists-p (username)
  (let ((response (get-document *db* "_design/accounts/_view/by_username" :key username)))
    (when (hashget response "rows")
      t)))

(defun confirm-password (account input)
  (when (string= (password account)
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
          (choose-character client))
        (progn
          (format client "~&Wrong password.~%")
          (maybe-login client)))))

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

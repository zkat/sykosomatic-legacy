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

;; game.lisp
;;
;; Implements a simple Sykosomatic-based game. Used for development and testing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage #:sykosomatic.game
  (:use :cl :sykosomatic.engine :sykosomatic.account :chillax :chillax.utils)
  (:export :begin-shared-hallucination))
(cl:in-package :sykosomatic.game)

(defgeneric engine-account-db (engine)
  (:documentation "Returns the database where ENGINE stores account information."))

(defgeneric ensure-account-db (engine)
  (:documentation "Makes sure the required account database for ENGINE exists. This function is
  required to be called before ENGINE-ACCOUNT-DB works."))

(defclass test-engine ()
  ((service-providers :reader engine-service-providers :initarg :providers)
   (runningp :reader engine-running-p :initform nil)
   (account-db :reader engine-account-db))
  (:default-initargs :providers nil))

(defmethod init-engine ((engine test-engine))
  (setf (slot-value engine 'runningp) t)
  (ensure-account-db engine)
  (format t "~&Engine ~A starting.~%" engine))

(defmethod update-engine ((engine test-engine))
  (let ((line (read-line)))
    (if (equalp "quit" line)
        (setf (slot-value engine 'runningp) nil)
        (progn (princ line) (fresh-line)))))

(defmethod teardown-engine ((engine test-engine))
  (format t "~&Engine ~A stopped.~%" engine))

(defun begin-shared-hallucination (&optional (engine (make-instance 'test-engine)))
  (run-engine engine))

;;;
;;; Accounts
;;;
(defun ensure-account-design-doc (engine &aux (db (engine-account-db engine)))
  (let ((existing-doc (handler-case
                          (get-document db "_design/accounts")
                        (document-not-found () nil)))
        (design-doc (mkhash "language" "common-lisp"
                            "views" (mkhash "by_username"
                                            (mkhash "map"
                                                    (prin1-to-string
                                                     '(lambda (doc &aux (type (at doc "type")))
                                                       (when (equal type "account")
                                                         (emit (at doc "username")
                                                               doc)))))))))
    (when existing-doc
      (setf (at design-doc "_rev") (at existing-doc "_rev")))
    (put-document db "_design/accounts" design-doc)))

(defmethod ensure-account-db ((engine test-engine))
  (setf (slot-value engine 'account-db)
        (ensure-db (make-instance 'yason-server) "sykosomatic-test"))
  (ensure-account-design-doc engine))

(defun find-account-document (db username)
  (let* ((response (get-document db "_design/accounts/_view/by_username" :key username))
         (rows (at response "rows")))
    (when rows (at (car rows) "value"))))

(defmethod find-account ((engine test-engine) username)
  (make-instance 'document :engine engine
                 :document
                 (find-account-document (engine-account-db engine) username)))

(defmethod ensure-account ((engine test-engine) username password email
                           &aux (db (engine-account-db engine)))
  (when (account-exists-p engine username)
    (error "That username is already being used."))
  (make-instance 'document
                 :engine engine
                 :db db
                 :document
                 (put-document db (gen-uuid db)
                               (mkhash "type" "account"
                                       "username" username
                                       "password" password
                                       "email" email))))

(defmethod account-username ((doc document))
  (document-slot-value doc "username"))
(defmethod account-password ((doc document))
  (document-slot-value doc "password"))
(defmethod account-email ((doc document))
  (document-slot-value doc "email"))
(defmethod account-engine ((doc document))
  (document-engine doc))

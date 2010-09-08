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
;; Account protocol for sykosomatic engines.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage #:sykosomatic.account
  (:use :cl)
  (:export :account-username :account-password :account-email :account-engine :find-account
           :ensure-account :hash-password :validate-username :validate-password :validate-email
           :verify-password :verify-password-using-engine :account-exists-p))
(cl:in-package :sykosomatic.account)

;;;
;;; Account protocol
;;;

;; Accessors
(defgeneric account-username (account))
(defgeneric account-password (account))
(defgeneric account-email (account))
(defgeneric account-engine (account))

(defgeneric find-account (engine username)
  (:documentation "Returns an account object associated with USERNAME. If there is no such account,
NIL is returned."))

(defgeneric ensure-account (engine username password email)
  (:documentation "Creates a new account."))

(defgeneric hash-password (engine password)
  (:documentation "Hashes PASSWORD. This is generally the form in which it will be stored."))

(defgeneric validate-username (engine username)
  (:method (engine username)
    (declare (ignore engine username))
    t))
(defgeneric validate-password (engine password)
  (:method (engine password)
    (declare (ignore engine password))
    t))
(defgeneric validate-email (engine email)
  (:method (engine email)
    (declare (ignore engine email))
    t))

(defgeneric verify-password-using-engine (engine account password)
  (:documentation "Returns a true value if PASSWORD is the valid password for ACCOUNT.")
  (:method (engine account password)
    (when (string= (account-password account)
                   (hash-password engine password))
      account)))

(defun verify-password (account password)
  "Trampoline function for VERIFY-ACCOUNT-USING-ENGINE. Confirms that PASSWORD is the correct
password for ACCOUNT."
  (verify-password-using-engine (account-engine account) account password))

(defgeneric account-exists-p (engine username)
  (:documentation "Returns a true value if an account associated with USERNAME exists.")
  (:method (engine username)
    (find-account engine username)))

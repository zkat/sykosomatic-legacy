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

;; character-creation.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defclass body (document) ())

(def-doc-accessors body
  (name "name")
  (description "description"))

(defun make-body (name description)
  (let ((uuid (gen-uuid)))
    (put-document *db* uuid
                  (mkhash "name" name
                          "description" description))
    (make-instance 'body :document (get-document *db* uuid))))

(defun/cc choose-character (client)
  (unless (bodies (account (soul client)))
    (format client "~&You have no characters.~%")
    (create-character client))
  (format client "~&The following characters are available:~%")
  (print-bodies client)
  (let ((choice (parse-integer (string-cleanup (prompt-client client "~&Choice: ")) :junk-allowed t))
        (bodies (bodies (account (soul client)))))
    (cond ((or (null choice)
               (not (<= 0 choice (length bodies))))
           (format client "~&That is not a valid choice.~%")
           (choose-character client))
          (t
           (enter-body client
                       (make-instance 'body
                                      :document
                                      (get-document *db* (elt bodies (1- choice)))))))))

(defun enter-body (client body)
  (setf (body (soul client)) body)
  (format client "~&You are now logged in as ~A.~%" (name body))
  (format client "~&Commands: 'look' and 'quit'. Type anything else to chat.~%")
  (broadcast-to-room client "~&~A enters the world.~%" (name body)))

(defun print-bodies (client)
  (let ((bodies (bodies (account (soul client)))))
    (loop for body-id in bodies
       for body = (make-instance 'body :document (get-document *db* body-id))
       for i from 1
       do (format client "~&~A. ~A - ~A~%" i (name body) (description body)))))

(defun/cc create-character (client)
  (let ((body (make-body (prompt-client client "~&Pick a name for your character: ")
                         (prompt-client client "~&Describe your character: ")))
        (account (account (soul client))))
    (setf (body (soul client)) body)
    (update account)
    (push (uuid body) (bodies account))
    (save account)))

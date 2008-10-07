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

;; avatar.lisp
;;
;; Character creation, character management, character login, etc.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :org.sykosomatic.core)

;;;
;;; Character Selection
;;;

(defun/cc choose-avatar (client)
  (with-accessors ((account account))
      client
    (write-to-client client "~&Choose a character: ~%")
    (write-to-client client "---------------------~%")
    (write-avatar-list client)
    (write-to-client client "---------------------~%")
    (write-to-client client "[N]ew character~%")
    (write-to-client client "---------------------~%")
    (write-to-client client "[B]ack~%")
    (write-to-client client "[Q]uit~%")
    (let ((choice (prompt-client client "Your choice: ")))
      (cond ((numberp (read-from-string choice))
	     (let* ((number-choice (read-from-string choice))
		    (avatar (unless (> number-choice (length (avatars account))) 
			      (elt (avatars account) (1- number-choice)))))
	       (setf (client avatar) client)
	       (setf (avatar client) avatar)
	       (initialize-avatar avatar)
	       (avatar-main-loop avatar)))
	    ((string-equal choice "b")
	     (account-menu client))
	    ((string-equal choice "q")
	     (disconnect-client client))
	    ((string-equal choice "n")
	     (create-avatar client)
	     (choose-avatar client))
	    (t
	     (write-to-client client "~&Invalid choice, try again.~%")
	     (choose-avatar client))))))

(defun write-avatar-list (client)
  "Writes the avatar list to account"
  (let* ((account (account client))
	 (avatar-list (avatars account)))
    (loop 
       for avatar in avatar-list
       for i from 1
       do (write-to-client client "~d. ~a~%" i (name avatar)))))

;; temp
(defun/cc avatar-main-loop (avatar)
  (loop
   (handle-avatar-input avatar)))

(defun/cc handle-avatar-input (avatar)
  (let ((input (prompt-client (client avatar) "")))
    (if (string-equal input "quit")
	(disconnect-avatar avatar)
	(process-avatar-input avatar input))))

;;;
;;; Character Creation
;;;
(defun/cc create-avatar (client)
  (let ((name (prompt-client client "~&Name your character: "))
	(account (account client)))
    (with-transaction ()
      (pushnew (make-instance '<avatar> :name name :account account) 
	       (avatars account)))
    (write-to-client client "~&Generic character created~%~%")))



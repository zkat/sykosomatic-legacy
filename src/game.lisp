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
  (:use :cl :sykosomatic.engine :sykosomatic.account)
  (:export :begin-shared-hallucination))
(cl:in-package :sykosomatic.game)

(defclass test-engine ()
  ((service-providers :reader engine-service-providers :initarg :providers)
   (runningp :reader engine-running-p :initform nil))
  (:default-initargs :providers nil))

(defmethod init-engine ((engine test-engine))
  (setf (slot-value engine 'runningp) t)
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

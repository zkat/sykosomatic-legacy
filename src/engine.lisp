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

;; engine.lisp
;;
;; Main file in sykosomatic. Contains the main loop.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

(defvar *main-thread*)
;;;
;;; Main
;;;
(defun begin-shared-hallucination ()
  (log-message :ENGINE "Initializing SykoSoMaTIC Core Engine.")
  (start-server)
  (init-database)
  (setf *main-thread*
	(bordeaux-threads:make-thread 
	 #'(lambda ()
	     (main-loop)) :name "sykosomatic-main-thread")))

(defun main-loop ()
  (process-event *main-event-queue*)
  (sleep 0.001)
  (main-loop))

(defun shutdown-shared-hallucination ()
  (log-message :ENGINE "Stopping SykoSoMaTIC Core Engine.")
  (sleep 1)
  ;; (lock-event-queue *main-event-queue*)
  ;; (process-remaining-events *main-event-queue*)
  (stop-server)
  (close-store)
  (when *main-thread*
    (bordeaux-threads:destroy-thread *main-thread*)
    (setf *main-thread* nil)))


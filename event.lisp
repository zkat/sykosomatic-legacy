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

;; event.lisp
;;
;; Contains the event class, and the code to handle the events directly. For
;; stuff related to the event queue, refer to event-queue.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Event class
;;;

(defclass <event> ()
  ((payload
    :initarg :payload
    :accessor payload
    :documentation "A function (lambda), that contains the code to be executed.")
   (exec-time
    :initarg :exec-time
    :initform (get-universal-time)
    :accessor exec-time
    :documentation "The delay, in seconds, until this event is supposed to fire.")))

(defun make-event (payload &key (delay 0))
  (make-instance '<event> 
		 :payload payload 
		 :exec-time (+ (get-universal-time) delay)))

;;;
;;; Event processing
;;;

;; (defun process-event ()
 
;; )
(defun execute-event (event)
  "Executes an event."
  (funcall (payload event)))

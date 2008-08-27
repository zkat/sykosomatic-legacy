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

;; queue.lisp
;;
;; Thread-safe queues. Taken from Monster Mountain's (with permission), since they're quite nice.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Queue
;;;
(defclass queue ()
  ((contents 
    :accessor queue-contents 
    :initform nil)
   (lock 
    :reader queue-lock 
    :initform (bordeaux-threads:make-lock))
   (condition 
    :reader queue-condition 
    :initform (bordeaux-threads:make-condition-variable))))

(defun %queue-empty-p (queue)
  (null (queue-contents queue)))

(defun %dequeue (queue)
  (pop (queue-contents queue)))

(defun %enqueue (queue obj)
  (setf (queue-contents queue) (nconc (queue-contents queue) (list obj)))
  queue)

(defun make-empty-queue ()
  "Returns a freshly created empty queue."
  (make-instance 'queue))

(defun enqueue (queue obj)
  "Adds an element to the queue."
  (bordeaux-threads:with-lock-held ((queue-lock queue))
    (%enqueue queue obj)
    (bordeaux-threads:condition-notify (queue-condition queue))))

(defun dequeue (queue &optional default)
  "Removes an element from the queue. Returns DEFAULT If the queue is empty."
  (bordeaux-threads:with-lock-held ((queue-lock queue))
    (if (%queue-empty-p queue)
	default
	(%dequeue queue))))

(defun blocking-dequeue (queue)
  "Removes an element from the queue. If the queue is empty, blocks until
another thread adds an element."
  (bordeaux-threads:with-lock-held ((queue-lock queue))
    (if (%queue-empty-p queue)
	(loop (bordeaux-threads:condition-wait (queue-condition queue) (queue-lock queue))
	      (unless (%queue-empty-p queue)
		(return (%dequeue queue))))
	(%dequeue queue))))

(defun queue-empty-p (queue)
  "True if the given queue is empty; false otherwise."
  (bordeaux-threads:with-lock-held ((queue-lock queue))
    (%queue-empty-p queue)))
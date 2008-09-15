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

;; logger.lisp
;;
;; contains a function that can be called from anywhere (even threads), and writes messages
;; to a log file in the .sykosomatic directory.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Logging facility
;;;
(defvar *log-lock* (bordeaux-threads:make-lock))

(defun log-message (type format-string &rest format-args)
  "Logs a message into a log file"
  (let ((message (apply #'format nil format-string format-args)))
  ;; TODO - make it so this function can take a format-string, and format-args. ;Maybe this is good enough.
  ;; TODO - make this write to different files, depending on the type.
    (multiple-value-bind (second minute hour date month year) (get-decoded-time)
      (bordeaux-threads:with-lock-held (*log-lock*)
	(with-open-file (s (ensure-directories-exist (merge-pathnames "sykosomatic.log" *log-directory*))
			   :direction :output 
			   :if-does-not-exist :create
			   :if-exists :append)
	  (format s 
		  "[~4,'0D/~2,'0D/~2,'0D][~2,'0D:~2,'0D:~2,'0D] -- ~a: ~a~%" 
		  year month date hour minute second type message))))))
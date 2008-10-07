
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

;; db.lisp
;;
;; Does stuff with the main game db.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic.core)

(defun init-database ()
  (make-instance 'mp-store :directory *db-directory*
		 :subsystems (list (make-instance 'store-object-subsystem)))
  (load-vocabulary)
  (setf *main-event-queue* (make-priority-queue :key #'exec-time))
  (unless *newbie-area*
    (if (all-rooms)
	(setf *newbie-area* (car (last (all-rooms))))
	(setf *newbie-area* (create-newbie-area)))))

(defun create-newbie-area ()
  (make-instance '<room> 
		 :name "closet" :adjectives (list "scary" "dark")
		 :desc "You can't see much of anything in this broom closet."
		 :features (list (make-instance '<game-object> :name "flask" :adjectives (list "ye")
						:desc "A flask with the word 'Ye' imprinted on it."))))



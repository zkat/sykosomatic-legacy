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

;; config.lisp
;;
;; Configuration file. Contains variables that handle configuration-related things. Right now,
;; it's just directories to which things are saved. By default, the game data directory is saved
;; to .sykosomatic, in the user's home directory.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :org.sykosomatic.core)

;;;
;;; Values
;;;

;;; Directories
(defvar *game-directory* (merge-pathnames #P".sykosomatic/" (user-homedir-pathname))
  "Configures the base directory for the game.")

(defvar *db-directory* (merge-pathnames #P"db/" *game-directory*)
  "Database directory.")

(setf *vocab-directory* (merge-pathnames #P"vocab/" *game-directory*))

(setf *log-directory* (merge-pathnames #P"logs/" *game-directory*))

;;; Server options

;;; Newbies
(defvar *newbie-area* nil
  "Room where avatars are dropped into by default")

;;; Main
(setf *main-client-function* #'(lambda (client) (login-menu client)))


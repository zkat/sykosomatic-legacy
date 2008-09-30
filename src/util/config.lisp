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
;; it's just directories to which things are saved. It is recommended that only *game-directory*
;; be changed, if possible. By default, the game data directory is saved to .sykosomatic, in the
;; user's home directory.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

;;;
;;; Values
;;;

;;; Directories
(defvar *game-directory* (merge-pathnames #P".sykosomatic/" (user-homedir-pathname))
  "Configures the base directory for the game.")

(defvar *rooms-directory* (merge-pathnames #P"rooms/" *game-directory*))
(defvar *players-directory* (merge-pathnames #P"players/" *game-directory*))
(defvar *vocab-directory* (merge-pathnames #P"vocab/" *game-directory*))
(defvar *log-directory* (merge-pathnames #P"logs/" *game-directory*))

;;; Server options
(defvar *default-server-address* "0.0.0.0")
(defvar *default-server-port* 4000)
(defvar *max-client-idle-time* (* 60 20)
  "How many seconds is a client allowed to idle before they're disconnected by the server?")

;;; Main
(defvar *main-function* (lambda (client) (login-client client)))

;;;
;;; Functions
;;;

;; (defun read-config-from-file (file)
;;   "Reads a configuration FILE and loads the appropriate values into their respective globals"
;;   )


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
(in-package :sykosomatic)

;; initialize the database
(make-instance 'mp-store :directory *db-directory*
 :subsystems (list (make-instance 'store-object-subsystem)))

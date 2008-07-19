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

;;;; Kat M
;;;; zkat@gmail.com
;;;; June/July 2008
;;;; 
;;;; A little text adventure game written in common lisp.
;;;;

(asdf:defsystem #:sykosomatic
  :depends-on (#:cl-ppcre)
  :depends-on (#:cl-store)	     
  :components ((:file "packages")
	       (:file "config")
	       (:file "db"
		      :depends-on ("config"))
	       (:file "classes"
		      :depends-on ("packages"))
	       (:file "player"
		      :depends-on ("packages"
				   "classes"
				   "db"))
	       (:file "map"
		      :depends-on ("packages"
				   "classes"
				   "player"
				   "db"))
	       (:file "parser"
		      :depends-on ("player"
				   "packages"
				   "classes"
				   "db"))))



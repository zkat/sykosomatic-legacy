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

(asdf:defsystem #:sykosomatic
  :name "SykoSoMaTIC"
  :author "Kat M <zkat@gmail.com>"
  :version "nil"
  :maintainer "Kat M <zkat@gmail.com>"
  :description "Sykopomp's Sodomizingly Masterful Text in Console"
  :long-description "A heavily-extensible, simple, text-adventure engine."
  :depends-on (#:cl-ppcre #:cl-store)
  :components ((:file "packages")
	       (:file "config")
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
				   "db"))
	       (:file "db"
		      :depends-on ("config")
		      :depends-on ("classes")
		      :depends-on ("map")
		      :depends-on ("player")
		      :depends-on ("parser"))
	       (:file "game"
		      :depends-on ("db"))))

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
  :description "Sykopomp's Somewhat Masterful Text in Console"
  :long-description "A heavily-extensible, simple, powerful text-adventure engine."
  :depends-on (#:cl-ppcre #:cl-store #:usocket #:bordeaux-threads #:cl-cont)
  :components ((:file "packages")
	       (:file "queue")
	       (:file "event")
	       (:file "event-queue")
	       (:file "config"
		      :depends-on ("packages"))
	       (:file "vocabulary"
		      :depends-on ("config"))
	       (:file "logger"
		      :depends-on ("config"))
	       (:file "game-object"
		      :depends-on ("config"))
	       (:file "mobile"
		      :depends-on ("game-object"))
	       (:file "item"
		      :depends-on ("game-object"))
	       (:file "parser"
		      :depends-on ("packages" "vocabulary"))
	       (:file "binder"
		      :depends-on ("db" "player" "room" "parser"))
	       (:file "commands"
		      :depends-on ("binder"))
	       (:file "game"
		      :depends-on ("commands"))
	       (:file "server"
		      :depends-on ("queue"))
	       (:file "client"
		      :depends-on ("server" "queue" "event-queue"))
	       (:file "room"
		      :depends-on ("game-object"))
	       (:file "player"
		      :depends-on ("mobile" "client"))
	       (:file "account"
		      :depends-on ("client"))))

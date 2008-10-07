;; This file is part of sykosomatic

(asdf:defsystem #:org.sykosomatic.core
  :name "SykoSoMaTIC"
  :author "Kat Marchan <kzm@sykosomatic.org>"
  :version "When it's done."
  :maintainer "Kat Marchan <kzm@sykosomatic.org>"
  :description "Sykopomp's Somewhat Masterful Text in Console"
  :long-description "A heavily-extensible, simple, powerful text-based online game engine. Core engine."
  :license "AGPL, see COPYING"
  :depends-on (#:cl-ppcre #:cl-store #:usocket #:bordeaux-threads #:fiveam
	       #:cl-cont #:ironclad #:bknr.datastore #:bknr.indices)
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "config")

	     (:module objects
		      :serial t
		      :components
		      ((:file "game-object")
		       (:file "room")
		       (:file "entity")
		       (:file "mobile")
		       (:file "item")
		       (:file "avatar")))
	     
	     (:module login
		      :serial t
		      :components
		      ((:file "account")
		       (:file "login")
		       (:file "login-avatar")))
	     
	     (:module commands
		      :serial t
		      :components
		      ((:module parser
				:serial t
				:components
				((:file "vocabulary")
				 (:file "parser")))
		       (:file "event")
		       (:file "binder")
		       (:file "commands")))

	     (:module database
		      :serial t
		      :components
		      ((:file "db")))
	     
	     (:file "engine")))))



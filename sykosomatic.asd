;; This file is part of sykosomatic

(asdf:defsystem #:sykosomatic
  :name "SykoSoMaTIC"
  :author "Kat M <kzm@sykosomatic.org>"
  :version "nil"
  :maintainer "Kat M <kzm@sykosomatic.org>"
  :description "Sykopomp's Somewhat Masterful Text in Console"
  :long-description "A heavily-extensible, simple, powerful text-based online game engine."
  :license "AGPL, see COPYING"
  :depends-on (#:cl-ppcre #:cl-store #:usocket #:bordeaux-threads 
	       #:cl-cont #:ironclad #:xmls #:fiveam #:bknr.datastore #:bknr.indices)
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")

	     (:module util
		      :serial t
		      :components
		      ((:file "config")
		       (:file "queue")
		       (:file "priority-queue")
		       (:file "xml-import")
		       (:file "logger")))

	     (:module objects
		      :serial t
		      :components
		      ((:file "game-object")
		       (:file "room")
		       (:file "entity")
		       (:file "mobile")
		       (:file "item")
		       (:file "avatar")))
	     
	     (:module network
		      :serial t
		      :components
		      ((:file "server")
		       (:file "client")
		       (:file "account")
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
	     
	     (:module tests
		      :serial t
		      :components
		      ((:file "account")
		       (:file "parser")
		       (:file "xml-import")))))))

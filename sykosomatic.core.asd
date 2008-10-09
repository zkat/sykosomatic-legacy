;; This file is part of sykosomatic

(asdf:defsystem #:sykosomatic.core
  :name "SykoSoMaTIC"
  :author "Kat Marchan <kzm@sykosomatic.org>"
  :version "When it's done."
  :maintainer "Kat Marchan <kzm@sykosomatic.org>"
  :description "Sykopomp's Somewhat Masterful Text in Console"
  :long-description "A heavily-extensible, simple, powerful text-based online game engine. Core engine."
  :license "AGPL, see COPYING"
  :depends-on (#:sykosomatic.util #:sykosomatic.network #:sykosomatic.parser #:sykosomatic.object
	       #:cl-cont #:cl-ppcre #:cl-store  #:bordeaux-threads #:ironclad #:bknr.datastore 
	       #:bknr.indices)
  :components 
  ((:module src
	    :serial t
	    :components
	    ((:file "packages")
	     (:file "config")

	     (:module login
		      :serial t
		      :components
		      ((:file "account")
		       (:file "login")
		       (:file "login-avatar")))
	     
	     (:module commands
		      :serial t
		      :components
		      ((:file "event")
		       (:file "binder")
		       (:file "commands")))

	     (:module database
		      :serial t
		      :components
		      ((:file "db")))
	     
	     (:file "engine")))))



;; This file is part of sykosomatic

(asdf:defsystem #:sykosomatic.object
  :name "SykoSoMaTIC Object System"
  :author "Kat Marchan <kzm@sykosomatic.org>"
  :version "When it's done."
  :maintainer "Kat Marchan <kzm@sykosomatic.org>"
  :description "Base object system for SykoSoMaTIC"
  :license "AGPL, see COPYING"
  :depends-on (#:sykosomatic.util #:sykosomatic.network #:bknr.datastore #:bknr.indices)
  :components
  ((:module src
	    :serial t
	    :components
	    ((:module objects
		      :serial t
		      :components
		      ((:file "game-object")
		       (:file "room")
		       (:file "entity")
		       (:file "mobile")
		       (:file "item")
		       (:file "avatar")))))))

;; This file is part of sykosomatic-ext

(asdf:defsystem sykosomatic-ext
  :version "0"
  :description "Standard extensions for SykoSoMaTIC"
  :maintainer "Kat <zkat@Dagon>"
  :author "Kat <zkat@Dagon>"
  :licence "GPLv3"
  :depends-on (#:sykosomatic #:cl-ppcre #:ironclad)
  :serial t
  :components
  ((:module ext
	    :serial t
	    :components
	    ((:file "packages")
	     (:module src
		      :serial t
		      :components
		      ((:module account
				:serial t
				:components
				((:file "account")))
		       (:module commands
				:serial t
				:components
				((:file "binder")
				 (:file "commands")))))))))

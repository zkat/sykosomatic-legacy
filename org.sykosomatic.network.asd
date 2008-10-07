;; This file is part of sykosomatic

(asdf:defsystem #:org.sykosomatic.network
    :name "SykoSoMaTIC Network"
    :author "Kat Marchan <kzm@sykosomatic.org>"
    :version "1.0"
    :maintainer "Kat Marchan <kzm@sykosomatic.org>"
    :description "SykoSoMaTIC's standard server/client library."
    :license "AGPL, see COPYING"
    :depends-on (#:org.sykosomatic.util #:usocket #:bordeaux-threads #:cl-cont)
    :components
    ((:module src
	      :serial t
	      :components
	      ((:module util
			:serial t
			:components
			((:file "packages")
			 (:file "server")
			 (:file "client")))))))


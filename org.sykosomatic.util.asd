;; This file is part of sykosomatic

(asdf:defsystem #:org.sykosomatic.util
    :name "SykoSoMaTIC Utilities"
    :author "Kat Marchan <kzm@sykosomatic.org>"
    :version "1.0"
    :maintainer "Kat Marchan <kzm@sykosomatic.org>"
    :license "AGPL, see COPYING"
    :depends-on (#:bordeaux-threads)
    :components
    ((:module src
	      :serial t
	      :components
	      ((:module network
			:serial t
			:components
			((:file "packages")
			 (:file "queue")
			 (:file "priority-queue")
			 (:file "logger")))))))


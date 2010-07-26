;; This file is part of sykosomatic

(asdf:defsystem #:sykosomatic.test
    :name "SykoSoMaTIC Test"
    :version "0"
    :description "Unit tests for SykoSoMaTIC"
    :maintainer "Kat Marchan <kzm@sykosomatic.org>"
    :author "Kat Marchan <kzm@sykosomatic.org>"
    :licence "AGPL"
    :depends-on (#:fiveam #:sykosomatic.core)
    :components
    ((:module tests
	      :serial t
	      :components
	      ((:file "packages")
	       (:file "account")
	       (:file "parser")
	       (:file "xml-import")))))
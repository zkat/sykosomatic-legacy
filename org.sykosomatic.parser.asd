;; This file is part of sykosomatic

(asdf:defsystem #:org.sykosomatic.parser
    :name "SykoSoMaTIC Parser"
    :author "Kat Marchan <kzm@sykosomatic.org>"
    :version "When it's done."
    :maintainer "Kat Marchan <kzm@sykosomatic.org>"
    :description "SykoSoMaTIC Standard User Input Parser."
    :license "AGPL, see COPYING"
    :depends-on (#:cl-ppcre #:bordeaux-threads)
    :components
    ((:module parser
	      :serial t
	      :components
	      ((:file "packages")
	       (:file "vocabulary")
	       (:file "parser")))))

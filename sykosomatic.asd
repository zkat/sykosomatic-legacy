(asdf:defsystem sykosomatic
  :version "0"
  :description ""
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "AGPL"
  :depends-on (alexandria iolib cl-ppcre cl-store trivial-gray-streams)
  :serial t
  :components
  ((module "src"
           :serial t
           :components
           ((:file "packages")
            (:file "vocabulary")
            (:file "parser")
            (:file "engine")
            (:file "tcp-service-provider")))))



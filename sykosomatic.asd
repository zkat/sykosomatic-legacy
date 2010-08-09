(asdf:defsystem sykosomatic
  :version "0"
  :description ""
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "AGPL"
  :depends-on (alexandria iolib cl-ppcre cl-store
               trivial-gray-streams ironclad cl-cont
               chillax anaphora)
  :serial t
  :components
  ((module "src"
           :serial t
           :components
           ((:file "packages")
            (:file "documents")
            (:file "location")
            (:file "vocabulary")
            (:file "parser")
            (:file "engine")
            (module "tcp-service-provider"
                    :serial t
                    :components
                    ((:file "config")
                     (:file "utils")
                     (:file "client")
                     (:file "character-creation")
                     (:file "account")
                     (:file "tcp-service-provider")))
            (:file "binder")
            (:file "game")))))

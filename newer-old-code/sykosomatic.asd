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
            (:file "verbs")
            (:file "parser")
            (:file "engine")
            (module "tcp-service-provider"
                    :serial t
                    :components
                    ((module "socket-server"
                             :serial t
                             :components
                             ((:file "packages")
                              (:file "utils")
                              (:file "tcp-socket-client")
                              (:file "tcp-socket-server")))))
            (:file "binder")
            (:file "game")))))


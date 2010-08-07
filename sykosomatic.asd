(asdf:defsystem sykosomatic
  :version "0"
  :description ""
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "AGPL"
  :depends-on (alexandria iolib cl-ppcre cl-store
               trivial-gray-streams ironclad cl-cont
               chillax)
  :serial t
  :components
  ((module "src"
           :serial t
           :components
           ((:file "packages")
            (:file "documents")
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
            (module "game-objects"
                    :serial t
                    :components
                    ((:file "game-object")
                     (:file "room")
                     (:file "mobile")
                     (:file "item")
                     (:file "human")))
            (:file "game")))))

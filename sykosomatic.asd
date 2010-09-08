(asdf:defsystem sykosomatic
  :version "0"
  :description "Text-based online game engine."
  :maintainer "Kat Marchán <kzm@sykosomatic.org>"
  :author "Kat Marchán <kzm@sykosomatic.org>"
  :licence "AGPLv3"
  :depends-on ()
  :components
  ((:module src
            :components
            ((:file "engine")
             (:file "account")))))

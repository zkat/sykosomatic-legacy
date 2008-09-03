;; This file is part of sykosomatic

(asdf:defsystem #:sykosomatic
  :name "SykoSoMaTIC"
  :author "Kat M <kzm@sykosomatic.org>"
  :version "nil"
  :maintainer "Kat M <kzm@sykosomatic.org>"
  :description "Sykopomp's Somewhat Masterful Text in Console"
  :long-description "A heavily-extensible, simple, powerful text-based online game engine."
  :depends-on (#:cl-ppcre #:cl-store #:usocket #:bordeaux-threads #:cl-cont #:ironclad)
  :components ((:file "packages")
	       (:file "queue" :depends-on ("packages"))
	       (:file "event" :depends-on ("packages"))
	       (:file "priority-queue" :depends-on ("packages"))
	       (:file "config" :depends-on ("packages"))
	       ;;server/client
	       (:file "logger" :depends-on ("config"))
	       (:file "server" :depends-on ("queue"))
	       (:file "client" :depends-on ("server" "queue" "priority-queue"))
	       ;;game-object stuff
	       (:file "game-object" :depends-on ("config"))
	       (:file "room" :depends-on ("game-object"))
	       (:file "entity" :depends-on ("game-object" "room"))
	       (:file "mobile" :depends-on ("entity"))
	       (:file "item" :depends-on ("entity"))
	       (:file "player" :depends-on ("mobile" "client"))
	       ;;parser/vocab stuff
	       (:file "vocabulary" :depends-on ("config"))
	       (:file "parser" :depends-on ("vocabulary"))
	       (:file "binder" :depends-on ("parser" "game-object" "player" "room"))
	       (:file "commands" :depends-on ("binder"))
	       ;; other stuff
	       (:file "account" :depends-on ("client" "player"))
	       (:file "xml-import" :depends-on ("entity"))))

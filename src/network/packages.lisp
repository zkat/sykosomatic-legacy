;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

(defpackage #:org.sykosomatic.network
  (:use :cl :cl-cont :bordeaux-threads :usocket)
  (:export
   
   ;; server.lisp
   :*default-server-address*
   :*default-server-port*
   :*max-client-idle-time*
   :*main-client-function*
   :*server*
   :start-server
   :stop-server
   :clients

   ;;client.lisp
   :<client>
   :ip
   :last-active
   :remove-client
   :client-idle-time
   :client-disconnected-error
   :read-line-from-client
   :prompt-client
   :client-y-or-n-p
   :write-to-client
   :write-to-all-clients
   :client-main))
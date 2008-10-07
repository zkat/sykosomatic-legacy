;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

;; sykosomatic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sykosomatic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with sykosomatic.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
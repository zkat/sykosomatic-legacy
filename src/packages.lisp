;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

;; sykosomatic is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; sykosomatic is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with sykosomatic.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:sykosomatic
  (:use :cl :cl-store :cl-cont :xmls)
  (:import-from :bordeaux-threads :make-thread :destroy-thread :all-threads :with-lock-held)

  ;; From parser
  (:export :*verbs*
	   :save-vocabulary
	   :load-vocabulary
	   :parse-string)
  ;; From network
  (:export :start-server
	   :stop-server
	   :ip
	   :client-idle-time
	   :account
	   :avatar
	   :read-line-from-client
	   :prompt-client
	   :client-y-or-n-p
	   :write-to-client
	   :write-to-all-clients)
  ;;Game classes
  (:export :<game-object>
	   :obj->file
	   :obj-list->files-in-dir
	   :save-objects
	   :file->obj
	   :files-in-path->obj-list
	   :load-objects)
  (:export :<entity>
	   :whereis
	   :put-entity)
  (:export :<mobile>)
  (:export :<item>)
  (:export :<mobile>)
  (:export :<player>
	   :*players*
	   :make-player
	   :player-p
	   :get-players
	   :write-to-player
	   :write-to-others-in-room
	   :disconnect-player
	   :save-players
	   :load-players)
  (:export :<room>
	   :*rooms*
	   :make-room
	   :<door>
	   :make-door
	   :set-exit
	   :save-rooms
	   :load-rooms)
  ;; events
  (:export :<event>
	   :make-event
	   :process-event
	   :execute-event)
  (:export :log-message))


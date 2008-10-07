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

(defpackage #:sykosomatic
  (:use :cl :cl-cont :xmls :fiveam :bknr.datastore :bknr.indices)
  (:import-from :bordeaux-threads :make-thread :destroy-thread :all-threads :with-lock-held)
  (:import-from :ironclad :byte-array-to-hex-string :digest-sequence :ascii-string-to-byte-array)
  (:export

   ;; config.lisp
   :*game-directory*
   :*db-directory*
   :*vocab-directory*
   :*log-directory*
   :*default-server-address*
   :*default-server-port*
   :*max-client-idle-time*
   :*newbie-area*
   :*main-function*

   ;; logger.lisp
   :log-message

   ;; objects module
   ;; --------------

   ;; game-object.lisp
   :<game-object>
   :name
   :objects-with-name
   :all-objects
   :aliases
   :adjectives
   :desc
   :features
   :short-description
   :long-description
   :write-to-target

   ;; room.lisp
   :<room>
   :rooms-with-name
   :all-rooms
   :contents
   :<exit>
   :exits-with-name
   :all-exits
   :open-p
   :locked-p
   :next-room
   :room-p
   :exit-p
   :get-exits
   :remove-object-from-room
   :put-object-in-room
   :write-to-others-in-room
   :write-to-room

   ;; entity.lisp
   :<entity>
   :entities-with-name
   :all-entities
   :location
   :invul-p
   :hp
   :whereis
   :put-entity

   ;; mobile.lisp
   :<mobile>
   :mobs-with-name
   :all-mobs
   :species
   :mobs-of-species
   :killcount
   :level
   :mobs-with-level
   :skills
   :inventory

   ;; item.lisp
   :<item>
   :items-with-name
   :all-items
   :equip-p
   :moveable-p
   :effects

   ;; avatar.lisp
   :<avatar>
   :avatar-with-name
   :all-avatars
   :last-location
   :client
   :account
   :avatar-p
   :get-avatars
   :initialize-avatar
   :disconnect-avatar   ))

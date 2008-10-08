;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

(defpackage #:sykosomatic.core
  (:use :cl :sykosomatic.util :sykosomatic.network :cl-cont
	:sykosomatic.parser :bknr.datastore :bknr.indices)
  (:import-from :bordeaux-threads :make-thread :destroy-thread :all-threads :with-lock-held)
  (:import-from :ironclad :byte-array-to-hex-string :digest-sequence :ascii-string-to-byte-array)
  (:export

   ;; general
   :defun/cc
   :define-persistent-class

   ;; config.lisp
   :*game-directory*
   :*db-directory*
   :*vocab-directory*
   :*log-directory*
   :*default-server-address*
   :*default-server-port*
   :*max-client-idle-time*
   :*newbie-area*
   :*main-client-function*

   ;; login module
   ;; ------------
   ;;
   ;; account.lisp
   :<account>
   :username
   :account-with-name
   :all-accounts
   :password
   :email
   :account-with-email
   :avatars
   :clients
   :account-type
   :accounts-with-type
   :known-ips
   :account-creation-error
   :account-exists-error

   ;; login-avatar.lisp
   :choose-avatar
   :avatar-main-loop
   :handle-avatar-input

   ;; login.lisp
   :login-menu
   :account-menu

   ;; objects module
   ;; --------------
   ;;
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
   :disconnect-avatar   
))

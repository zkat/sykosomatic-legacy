;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

(defpackage #:sykosomatic.object
  (:use :cl :sykosomatic.util :sykosomatic.network :bknr.datastore :bknr.indices)
  (:export
   
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

   ;; entity.lisp
   :<entity>
   :entities-with-name
   :all-entities
   :location
   :invul-p
   :hp
   :whereis
   :put-entity

   ;; item.lisp
   :<item>
   :items-with-name
   :all-items
   :equip-p
   :moveable-p
   :effects

   ;; mobile.lisp
   :<mobile>
   :mobs-with-name
   :all-mobs
   :species
   :mobs-of-species
   :killcount
   :mobs-with-level
   :skills
   :inventory

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
   :exits-that-lead-to
   :room-p
   :exit-p
   :get-exits
   :remove-object-from-room
   :put-object-in-room
   :write-to-others-in-room
   :write-to-room))
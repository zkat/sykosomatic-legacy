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
   
   ;; engine.lisp
   :begin-shared-hallucination
   :shutdown-shared-hallucination

   ;; database module
   ;; ---------------
   ;;
   ;; db.lisp
   :init-database

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
   :account-menu))

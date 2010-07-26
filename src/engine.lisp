;; Copyright 2008-2010 Kat Marchán

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

;; engine.lisp
;;
;; Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defgeneric init (obj))
(defgeneric teardown (obj))
(defgeneric update (obj))
(defgeneric run (obj))

(defclass engine ()
  ((service-providers :initform nil :accessor service-providers
                      :initarg :providers))
  (:documentation
   "The engine handles all the core logic and interactions. It communicates with
its service-providers through events."))

(defmethod init ((engine engine))
  (map nil #'init (service-providers engine)))
(defmethod teardown ((engine engine))
  (map nil #'teardown (service-providers engine)))
(defmethod update ((engine engine))
  (map nil #'update (service-providers engine)))

(defmethod run ((engine engine))
  (unwind-protect
       (progn
         (init engine)
         (loop (update engine)))
    (teardown engine)))

(defclass service-provider ()
  ()
  (:documentation
   "Service providers handle users. They translate user interactions into events,
which the associated engine can then handle."))

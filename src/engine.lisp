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
;; Engine protocol and base implementation.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage #:sykosomatic.engine
  (:use :cl)
  (:export :init-engine :engine-running-p :update-engine :teardown-engine
           :engine-service-providers :run-engine))
(cl:in-package :sykosomatic.engine)

;;;
;;; Engine API
;;;
(defgeneric init-engine (engine)
  (:documentation "Performs any initialization necessary before ENGINE can run."))

;; TODO: This could be specified better? -JM
(defgeneric engine-running-p (engine)
  (:documentation "Returns a generalized boolean if ENGINE is still active."))

(defgeneric update-engine (engine)
  (:documentation "Ticks ENGINE. Generally meant to be used as part of a main loop."))

(defgeneric teardown-engine (engine)
  (:documentation "Performs any cleanup that needs to be done after ENGINE stops running."))

(defgeneric engine-service-providers (engine)
  (:documentation "Returns a sequence of service providers associated with this engine."))

(defgeneric run-engine (engine)
  (:documentation "Runs the main loop for ENGINE.")
  (:method (engine)
    (unwind-protect
         (progn (init-engine engine)
                (loop while (engine-running-p engine)
                   do (update-engine engine))
                engine)
      (teardown-engine engine))))

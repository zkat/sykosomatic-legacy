;; Copyright 2008 Rudolf Olah

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

(in-package #:sykosomatic)

(def-suite account)

(def-suite sanity-checks :in account)
(in-suite sanity-checks)

(test username-sanity
      (is-true (confirm-username-sanity "hello"))
      (is-true (confirm-username-sanity "HELLO"))
      (is-true (confirm-username-sanity "HELLOworld"))
      (is-false (confirm-username-sanity "hello world"))
)

;;(test password-sanity
;;)

;;(test email-sanity
;;)
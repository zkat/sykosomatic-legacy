
;; Copyright 2008 Rudolf Olah

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

(in-package #:sykosomatic-tests)

(def-suite xml-import)

(test validation
      (is-true (xml-validate '("room" nil
			       ("name" "A name")
			       ("desc" "A description")
			       ("desc-long" "A long description"))))
)

(test validation-predicates
      (is-false (xml-valid-p nil "room"))
      (is-false (xml-valid-p '("room" nil '()) "room"))
      (is-false (xml-valid-p '("rooma" nil '()) "room"))
      (is-true (xml-valid-p '("room" nil
			      ("name" "My Name")
			      ("desc" "A description"))
			    "room"))
)
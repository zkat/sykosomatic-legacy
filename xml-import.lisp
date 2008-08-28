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

(in-package #:sykosomatic)

;;;; XML-IMPORT ;;;
;; Imports objects, rooms and items into sykosomatic. Includes
;; validation.

(defun xml-valid-room-p (element)
  (destructuring-bind (name _ children) element
    (let ((child-names (mapcar #'first children)))
      (and (string= name "room")
	   ;; Are all children valid choices?
	   (subsetp child-names '("name" "desc" "desc-long" "features")
		    :test #'string=)
	   ;; Can all the required children be found?
	   (subsetp '("name" "desc" "desc-long") child-names)))))
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

;; binder.lisp
;;
;; Binds ASTs to local, concrete objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defvar *actor*)
(defvar *tree*)
(defun invoke-syntax-tree (tree actor &aux
                           (*actor* actor)
                           (*package* (find-package :sykosomatic))
                           (tree (cdr tree))
                           (*tree* tree))
  (let ((verb (eval (read-from-string (function-definition (bind-verb (cdr (assoc :verb tree))))))))
    (funcall verb)))

(defun bind-verb (word)
  (find-verb word))

(defun bind-adverb (adverb)
  adverb)

(defun bind-noun-group (tree)
  ;; NOTE: Very lame about actually binding right now.
  (let* ((prep-phrase (cdr (assoc :prepositional-phrase tree)))
         (object (bind-noun-phrase (cdr (assoc :noun-phrase (cdr (assoc :object prep-phrase)))))))
    (values object)))

(defun bind-noun-phrase (noun-phrase)
  ;; LAAAAME
  (let ((noun (cdr (assoc :noun noun-phrase))))
    (find noun (objects-in-scope *actor*) :key #'name :test #'string=)))

(defun objects-in-scope (actor)
  (let ((location (get-location-by-id (location actor))))
    (cons location (mapcar #'get-body-by-id (contents location)))))

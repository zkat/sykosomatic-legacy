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

(defun bind-syntax-tree (actor tree &aux (*actor* actor) (tree (cdr tree)))
  (let ((verb (bind-verb (cdr (assoc :verb tree))))
        (adverb (bind-adverb (cdr (assoc :adverb tree))))
        (chat-string (cdr (assoc :chat-string tree)))
        (direct-object (bind-object (cdr (assoc :direct-object
                                                (cdr (assoc :noun-clause tree))))))
        (indirect-object (bind-object (cdr (assoc :indirect-object
                                                  (cdr (assoc :noun-clause tree)))))))
    `((:verb . ,verb)
      (:adverb . ,adverb)
      (:chat-string . ,chat-string)
      (:direct-object . ,direct-object)
      (:indirect-object . ,indirect-object))))

(defun bind-verb (word)
  (find-verb word))

(defun bind-adverb (adverb)
  adverb)

(defun bind-object (tree)
  ;; NOTE: Very lame about actually binding right now.
  (let* ((prep-phrase (cdr (assoc :prepositional-phrase tree)))
         (object (bind-noun-phrase (cdr (assoc :noun-phrase (cdr (assoc :object prep-phrase)))))))
    (values object)))

(defun bind-noun-phrase (noun-phrase)
  ;; LAAAAME
  (let ((noun (cdr (assoc :noun noun-phrase))))
    (find noun (objects-in-scope *actor*) :key #'name :test #'string=)))

(defun objects-in-scope (actor)
  (let* ((location-id (location actor))
         (location (make-instance 'location :document (get-document *db* location-id))))
    (cons location (mapcar (lambda (obj-id)
                             (make-instance 'body :document
                                            (get-document *db* obj-id)))
                           (contents location)))))

(defun invoke-bound-syntax-tree (actor tree &aux (*package* (find-package :sykosomatic)))
  (let* ((verb-body (print (read-from-string (function-definition (cdr (assoc :verb tree))))))
         (verb (eval verb-body))
         (direct-object (cdr (assoc :direct-object tree))))
    (funcall verb actor direct-object nil nil)))

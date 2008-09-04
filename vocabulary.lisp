;; Copyright 2008 Kat Marchan

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

;; vocabulary.lisp
;;
;; Contains variables that hold the vocabulary. Also handles loading/saving.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package #:sykosomatic)

;;;
;;; Vocab vars
;;;

;; Note: Could these entries be put into a single object, or a struct?

;; These are hash tables because they'll have thousands of entries.
(defvar *verbs* (make-hash-table :test #'equalp)
  "This is a dotted list right now. The CAR is a string, CDR the function.")

(defvar *adverbs* (make-hash-table :test #'equalp)
  "This contains a HASH TABLE of all available ADVERBS.")

;; These are relatively small, so they'll stay as conses.
(defvar *articles* nil
  "A list of articles, as strings.")

(defvar *prepositions* nil
  "A simple list of strings which represent prepositions.")

(defvar *pronouns* nil
  "List of pronouns, as strings.")

;; This seems unnecessary. Neither the binder nor the parser need such a list. Note that
;; this list is no longer handled in load/save
(defvar *adjectives* nil 
  "List of adjectives, as strings.")

;;;
;;; Load/Save
;;;

(defun save-vocabulary ()
  "Saves all the nice vocabulary words :)"
  (cl-store:store *articles* (ensure-directories-exist (merge-pathnames #P"articles.db" *vocab-directory*)))
  (cl-store:store *verbs* (ensure-directories-exist (merge-pathnames #P"verbs.db" *vocab-directory*)))
  (cl-store:store *adverbs* (ensure-directories-exist (merge-pathnames #P"adverbs.db" *vocab-directory*)))
  (cl-store:store *prepositions* (ensure-directories-exist (merge-pathnames #P"prepositions.db" *vocab-directory*)))
  (cl-store:store *pronouns* (ensure-directories-exist (merge-pathnames #P"pronouns.db" *vocab-directory*)))
  (format t "Vocabulary saved."))

(defun load-vocabulary ()
  "Loads saved vocab files into their respective variables."
  (setf *articles* (cl-store:restore (merge-pathnames #P"articles.db" *vocab-directory*)))
  (setf *verbs* (cl-store:restore (merge-pathnames #P"verbs.db" *vocab-directory*)))
  (setf *adverbs* (cl-store:restore (merge-pathnames #P"adverbs.db" *vocab-directory*)))
  (setf *prepositions* (cl-store:restore (merge-pathnames #P"prepositions.db" *vocab-directory*)))
  (setf *pronouns* (cl-store:restore (merge-pathnames #P"pronouns.db" *vocab-directory*)))
  (format t "Vocabulary loaded."))


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

;; vocabulary.lisp
;;
;; Contains variables that hold the vocabulary. Also handles loading/saving.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(declaim (optimize debug))

(defvar *vocabulary* (make-hash-table :test #'equalp)
  "A hash table holding the vocabulary for the game. Keys are strings mapping to a list of word types")

;;;
;;; Load/Save
;;;

(defun save-vocabulary ()
  "Saves all the nice vocabulary words :)"
  (format t "Vocabulary saved."))

(defun load-vocabulary ()
  "Loads saved vocab files into their respective variables."
  (format t "Vocabulary loaded."))

;;;
;;; Database Management
;;;
(defun remove-word (word)
  (remhash word *vocabulary*)
  word)

(defun add-category-to-word (string category)
  (pushnew category (gethash string *vocabulary*)))

(defun remove-category-from-word (string category)
  (deletef (gethash string *vocabulary*) category))

(defun category-in-word-p (string category)
  (when (find category (gethash string *vocabulary*))
    string))

(defun add-verb (word)
  "Associates STRING with FUNCTION and adds the new verb to *VERBS*"
  (add-category-to-word word :verb))

(defun remove-verb (word)
  "Removes the VERB that corresponds to STRING from *VERBS*"
  (remove-category-from-word word :verb))

(defun (setf verbp) (verbp word)
  (if verbp
      (add-verb word)
      (remove-verb word))
  verbp)

(defun verbp (word)
  "Is WORD a VERB?"
  (category-in-word-p word :verb))

(defun chat-string-p (word)
  "Is WORD a CHAT-WORD?"
  ;; Ew..
  (char-equal #\' (char word 0)))

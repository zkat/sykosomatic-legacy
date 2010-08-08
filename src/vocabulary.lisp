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

(defparameter *vocabulary-document-id* "vocabulary")

(defvar *vocabulary* nil
  "A hash table holding the vocabulary for the game. Keys are strings mapping to a list of word types")

;;;
;;; Load/Save
;;;
(defun copy-table-contents (orig dest)
  (maphash (lambda (k v)
             (setf (hashget dest k) v))
           orig)
  dest)

(defun init-vocabulary ()
  (setf *vocabulary* (make-hash-table :test #'equalp)))

(defun save-vocabulary ()
  "Saves all the nice vocabulary words :)"
  (handler-case
      (put-document *db* *vocabulary-document-id* *vocabulary*)
    (document-conflict ()
      (format t "~&Conflict while saving the current vocabulary. Loading existing one.~%")
      (return-from save-vocabulary (load-vocabulary))))
  (init-vocabulary)
  (handler-case
      (setf *vocabulary* (copy-hash-table (get-document *db* *vocabulary-document-id*) :test #'equalp))
    (document-not-found ()
      (format t "~&The vocabulary disappeared from under our noses!~%")))
  (format t "~&Vocabulary saved.~%")
  t)

(defun load-vocabulary ()
  "Loads saved vocab files into their respective variables."
  (init-vocabulary)
  (handler-case
      (setf *vocabulary* (copy-hash-table (get-document *db* *vocabulary-document-id*) :test #'equalp))
    (document-not-found ()
      (format t "~&No vocabulary document. Unable to load.~%")
      (return-from load-vocabulary nil)))
  (format t "~&Vocabulary loaded.~%")
  t)

;;;
;;; Database Management
;;;
(defun remove-word (word)
  (remhash word *vocabulary*)
  word)

(defun add-category-to-word (string category)
  (pushnew category (hashget *vocabulary* string)))

(defun remove-category-from-word (string category)
  (deletef (hashget *vocabulary* string) category))

(defun category-in-word-p (string category)
  (when (find category (hashget *vocabulary* string) :test #'string-equal)
    string))

(defun add-verb (word)
  "Associates STRING with FUNCTION and adds the new verb to *VERBS*"
  (add-category-to-word word "verb"))

(defun remove-verb (word)
  "Removes the VERB that corresponds to STRING from *VERBS*"
  (remove-category-from-word word "verb"))

(defun (setf verbp) (verbp word)
  (if verbp
      (add-verb word)
      (remove-verb word))
  verbp)

(defun verbp (word)
  "Is WORD a VERB?"
  (category-in-word-p word "verb"))

(defun chat-string-p (word)
  "Is WORD a CHAT-WORD?"
  ;; Ew..
  (char-equal #\' (char word 0)))

;; Copyright 2008 Kat Marchan

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
(in-package :org.sykosomatic.core)

;;;
;;; Vocab vars
;;;

(defvar *verbs* (make-hash-table :test #'equalp)
  "This is a dotted list right now. The CAR is a string, CDR the function.")

(defvar *adverbs* (make-hash-table :test #'equalp)
  "This contains a HASH TABLE of all available ADVERBS.")

(defvar *articles* (make-hash-table :test #'equalp)
    "A table of articles, as strings.")

(defvar *prepositions* (make-hash-table :test #'equalp)
  "A table of strings which represent prepositions.")

(defvar *pronouns* (make-hash-table :test #'equalp)
  "Table of pronouns, as strings.")

(defvar *conjunctions* (make-hash-table :test #'equalp)
  "Table of possible conjunctions. Go wild.")

(defvar *cardinal-numbers* (make-hash-table :test #'equalp)
  "Table of words representing cardinal numbers, and the number they map to.")

(defvar *ordinal-numbers* (make-hash-table :test #'equalp)
  "Table of words representing ordinal numbers, and the number they map to.")

(defvar *plural-exceptions* (make-hash-table :test #'equalp)
  "Table of exceptions to the English WORD+(e)s pluralization rule.")

;;;
;;; Load/Save
;;;

;; TODO: Clean these up. They suck.
(defun save-vocabulary ()
  "Saves all the nice vocabulary words :)"
  (cl-store:store *verbs* (ensure-directories-exist (merge-pathnames #P"verbs.db" *vocab-directory*)))
  (cl-store:store *adverbs* (ensure-directories-exist (merge-pathnames #P"adverbs.db" *vocab-directory*)))
  (cl-store:store *articles* (ensure-directories-exist (merge-pathnames #P"articles.db" *vocab-directory*)))
  (cl-store:store *prepositions* (ensure-directories-exist (merge-pathnames #P"prepositions.db" *vocab-directory*)))
  (cl-store:store *pronouns* (ensure-directories-exist (merge-pathnames #P"pronouns.db" *vocab-directory*)))
  (cl-store:store *conjunctions* (ensure-directories-exist (merge-pathnames #P"conjunctions.db" *vocab-directory*)))
  (cl-store:store *cardinal-numbers* (ensure-directories-exist (merge-pathnames #P"cardinal-numbers.db" *vocab-directory*)))
  (cl-store:store *ordinal-numbers* (ensure-directories-exist (merge-pathnames #P"ordinal-numbers.db" *vocab-directory*)))  
  (cl-store:store *plural-exceptions* (ensure-directories-exist (merge-pathnames #P"plural-exceptions.db" *vocab-directory*)))
  (format t "Vocabulary saved."))

(defun load-vocabulary ()
  "Loads saved vocab files into their respective variables."
  (setf *verbs* (cl-store:restore (merge-pathnames #P"verbs.db" *vocab-directory*)))
  (setf *adverbs* (cl-store:restore (merge-pathnames #P"adverbs.db" *vocab-directory*)))
  (setf *articles* (cl-store:restore (merge-pathnames #P"articles.db" *vocab-directory*)))
  (setf *prepositions* (cl-store:restore (merge-pathnames #P"prepositions.db" *vocab-directory*)))
  (setf *pronouns* (cl-store:restore (merge-pathnames #P"pronouns.db" *vocab-directory*)))
  (setf *conjunctions* (cl-store:restore (merge-pathnames #P"conjunctions.db" *vocab-directory*)))
  (setf *cardinal-numbers* (cl-store:restore (merge-pathnames #P"cardinal-numbers.db" *vocab-directory*)))
  (setf *ordinal-numbers* (cl-store:restore (merge-pathnames #P"ordinal-numbers.db" *vocab-directory*)))
  (setf *plural-exceptions* (cl-store:restore (merge-pathnames #P"plural-exceptions.db" *vocab-directory*)))
  (format t "Vocabulary loaded."))

;;;
;;; Database Management
;;;

(defun add-verb (string function)
  "Associates STRING with FUNCTION and adds the new verb to *VERBS*"
  (setf (gethash string *verbs*) function)
  (save-vocabulary))

(defun remove-verb (string)
  "Removes the VERB that corresponds to STRING from *VERBS*"
  (remhash string *verbs*)
  (save-vocabulary))

(defun refresh-verb (string function)
  "Associates STRING with FUNCTION and adds it to *VERBS*,
removing all previous associations with STRING"
  (remove-verb string)
  (add-verb string function)
  (save-vocabulary))

(defun add-emote (string)
  (add-verb string #'game-action-emote))

;;;
;;; Predicates
;;;

(defun article-p (word)
  "Is WORD an ARTICLE?"
  (gethash word *articles*))

(defun verb-p (word)
  "Is WORD a VERB?"
  (gethash word *verbs*))

(defun chat-string-p (word)
   "Is WORD a CHAT-WORD?"
   (unless (null word)
     (char-equal #\' (char word 0))))

(defun preposition-p (word)
  "Is WORD a PREPOSITION?"
  (gethash word *prepositions*))

(defun adverb-p (word)
  "Is WORD an ADVERB?"
  (gethash word *adverbs*))

(defun conjunction-p (word)
  "Is WORD a CONJUNCTION?"
  (gethash word *conjunctions*))

(defun possessive-p (word)
  "Is WORD in possessive form?
This function checks for s' or 's form of possessives in English."
  (or (string-equal word "my")
      (when (> (length word) 2)
	(let ((second-to-last-letter (elt word(- (length word) 2)))
	      (last-letter (elt word (- (length word) 1))))
	  (or (and (equal second-to-last-letter #\')
		   (equal last-letter #\s))
	      (and (equal second-to-last-letter #\s)
		   (equal last-letter #\')))))))

(defun pronoun-p (word)
  "Is WORD a PRONOUN?"
  (gethash word *pronouns*))

;; TODO: This should also deal with numbers in their "#th" form
(defun ordinal-number-p (word)
  "Is WORD an ORDINAL NUMBER?
This function checks for the full-word version,
as well as number+th/st/nd/rd form."
  (gethash word *ordinal-numbers*))

(defun cardinal-number-p (word)
  "Is WORD a CARDINAL NUMBER?
This function checks for the full-word version, as well as the plain number version."
  (gethash word *cardinal-numbers*))

;; TODO
(defun plural-p (word)
  "Is WORD in plural form?
This function checks both standard pluralization (WORD+s), plus a database of irregular verbs."
  word)


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
  (pushnew category (hashget *vocabulary* string) :test #'string-equal)
  (save-vocabulary))

(defun remove-category-from-word (string category)
  (deletef (hashget *vocabulary* string) category :test #'string-equal)
  (save-vocabulary))

(defun category-in-word-p (string category)
  (when (find category (hashget *vocabulary* string) :test #'string-equal)
    string))

(defclass verb (document) ())
(def-doc-accessors verb
  (function-definition "function_definition")
  (word "word"))

(defmethod (setf function-definition) :around (new-value (verb verb))
  (if (stringp new-value)
      (call-next-method)
      (call-next-method (prin1-to-string new-value) verb)))

(defun make-verb (word function-definition)
  (let ((uuid (gen-uuid)))
    (put-document *db* uuid
                  (mkhash "type" "verb"
                          "word" word
                          "function_definition" (prin1-to-string function-definition)))
    (make-instance 'verb :document (get-document *db* uuid))))

(defun ensure-verb-design-doc ()
  (or (handler-case
          (get-document *db* "_design/verbs")
        (document-not-found () nil))
      (put-document *db* "_design/verbs"
                    (mkhash "language" "common-lisp"
                            "views" (mkhash "by_word"
                                            (mkhash "map"
                                                    (prin1-to-string
                                                     '(lambda (doc &aux (type (hashget doc "type")))
                                                       (when (equal type "verb")
                                                         (emit (hashget doc "word")
                                                               (hashget doc "function_definition")))))))))))

(defun find-verb (word)
  (let* ((response (get-document *db* "_design/verbs/_view/by_word" :key word))
         (rows (hashget response "rows")))
    (when rows
      (make-instance 'verb
                     :document (get-document *db* (hashget (car rows) "id"))))))

(defun add-verb (word function-definition)
  "Associates STRING with FUNCTION and adds the new verb to *VERBS*"
  (add-category-to-word word "verb")
  (let ((existing-doc (find-verb word)))
    (if existing-doc
        (progn
          (setf (function-definition existing-doc) (prin1-to-string function-definition))
          (save existing-doc))
        (make-verb word function-definition))))

(defun remove-verb (word)
  "Removes the VERB that corresponds to STRING from *VERBS*"
  (remove-category-from-word word "verb"))

(defun verbp (word)
  "Is WORD a VERB?"
  (category-in-word-p word "verb"))

(defun chat-string-p (word)
  "Is WORD a CHAT-WORD?"
  ;; Ew..
  (char-equal #\' (char word 0)))

(defun articlep (word)
  (category-in-word-p word "article"))

(defun conjunctionp (word)
  (category-in-word-p word "conjunction"))

(defun prepositionp (word)
  (category-in-word-p word "preposition"))

(defun pronounp (word)
  (category-in-word-p word "pronoun"))

(defun adverbp (word)
  (category-in-word-p word "adverb"))

(defun ordinal-number-p (word)
  "Is WORD an ORDINAL NUMBER?
This function checks for the full-word version,
as well as number+th/st/nd/rd form."
  (or (category-in-word-p word "ordinal-number")
      (multiple-value-bind (integer stop)
          (parse-integer word :junk-allowed t)
        (and (numberp integer)
             (let ((subword (subseq word stop)))
               (or (string-equal subword "st")
                   (string-equal subword "nd")
                   (string-equal subword "rd")
                   (string-equal subword "th")))))))

(defun cardinal-number-p (word)
  "Is WORD a CARDINAL NUMBER?
This function checks for the full-word version, as well as the plain number version."
  (or (category-in-word-p word "cardinal-number")
      (multiple-value-bind (integer stop)
          (parse-integer word :junk-allowed t)
        (and (numberp integer)
             (zerop (length (subseq word stop)))))))

(defun possessivep (word)
  "Is WORD in possessive form?
This function checks for s' or 's form of possessives in English."
  (or (category-in-word-p word "possessive")
      (when (> (length word) 2)
        (let ((second-to-last-letter (elt word (- (length word) 2)))
              (last-letter (elt word (- (length word) 1))))
          (or (and (equal second-to-last-letter #\')
                   (equal last-letter #\s))
              (and (equal second-to-last-letter #\s)
                   (equal last-letter #\')))))))

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

;; verbs.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(defclass verb (document) ())
(def-doc-accessors verb
  (function-definition "function-definition")
  (word "word"))

(defmethod (setf function-definition) :around (new-value (verb verb))
  (if (stringp new-value)
      (call-next-method)
      (call-next-method (prin1-to-string new-value) verb)))

(defun make-verb (word function-definition)
  (make-instance 'verb :document
                 (save-document *db* (gen-uuid)
                                (mkhash "type" "verb"
                                        "word" word
                                        "function-definition"
                                        (prin1-to-string function-definition)))))

(defun ensure-verb-design-doc ()
  (aprog1 (handler-case
              (get-document *db* "_design/verbs")
            (document-not-found () nil))
    (let ((document (mkhash "language" "common-lisp"
                            "views" (mkhash "by_word"
                                            (mkhash "map"
                                                    (prin1-to-string
                                                     '(lambda (doc &aux (type (hashget doc "type")))
                                                       (when (equal type "verb")
                                                         (emit (hashget doc "word")
                                                               (hashget doc "function-definition"))))))))))
      (when it (setf (hashget document "_rev") (hashget it "_rev")))
      (save-document *db* "_design/verbs" document))))

(defun get-verb-body (word)
  (awhen (hashget
          (get-document *db* "_design/verbs/_view/by_word" :key word)
          "rows")
    (values (hashget (car it) "value"))))

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

(defmacro defverb (name &body body)
  `(add-verb (string-downcase (string ',name))
             '(lambda () ,@body)))

(defun handle-social-verb (infinitive 1psingular)
  (let ((soul (body->soul *actor*)))
    (cond (*chat-string*
           (format (client soul) "~&You ~A, \"~A\"~%" infinitive *chat-string*)
           (broadcast-to-others *actor* "~&~A ~A, \"~A\"~%" (name *actor*) 1psingular *chat-string*))
          (t
           (format (client soul) "~&You ~A.~%" infinitive)
           (broadcast-to-others *actor* "~&~A ~A.~%" (name *actor*) 1psingular)))))

(defun add-social-verb (infinitive 1psingular)
  (add-verb (string-downcase infinitive)
            `(lambda () (handle-social-verb ,infinitive ,1psingular))))

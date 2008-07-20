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

;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-

;;;; parser.lisp
;;;;
;;;; Contains the parser functions

(in-package #:sykosomatic)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  Parser  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pre-processing (data cleanup)
;;; -----------------------------
;;
(defun preprocess-string (string)
  "Get rid of trailing whitespace"
  (string-trim '(#\Space #\Tab #\Newline) string))

;;Do this later, when I have the rest of the parser working
(defun grammaritisize-chat-string (chat-string)
  "Cleans up the chat string and removes stupidity"
  )

;;; Tokenizer
;;; ---------
;; Notes: *This tokenizer (and the <word> class) assumes that there is only one pos per existing word.

;; Part 1: Raw string -> list of strings
;; ------
;
(defun split-command-string (command-string)
  "Splits each COMMAND in COMMAND-STRING and puts it in a list of words-strings."
  (cl-ppcre:all-matches-as-strings "[a-zA-Z0-9!@$%&']{1,}" command-string))

(defun split-off-chat-string (string)
  "Takes a raw STRING and returns a LIST with COMMAND-STRING and CHAT-STRING"
  (cl-ppcre:split "^+'| +'|\"" string :limit 2))

;; Part 2: List of strings -> list of word objects
;; -------
;
(defclass <word> ()
  ((word
    :initarg :word
    :accessor word
    :initform (error "Must supply a word")
    :documentation "A string containing the word")
   (pos
    :initarg :pos
    :accessor pos
    :initform (error "Must supply a part of speech in form '(:pos :pos :etc)")
    :documentation "The part of speech of this word")))

(defun word->word-obj (word-string)
  "Searches DB for <WORD> object match with WORD-STRING"
  (loop 
     for word-obj in *vocabulary*
     do (if (string-equal word-string (word word-obj))
	    (return-from word->word-obj word-obj)
	    (return-from word->word-obj word-string))))

(defun string->obj-list (string)
  "Takes a STRING and turns it into a list of parsable <word> objects"
  (let ((string-list (split-command-string string)))
    (loop
       for word in string-list
       collect (word->word-obj word))))

;; Parser
;; ------
;
(defclass parse-tree-node ()
  ((left-child
    :initarg :lc
    :initform nil
    :accessor left-child)
   (right-child
    :initarg :rc
    :initform nil
    :accessor right-child)
   (contents
    :initarg :contents
    :initform nil
    :accessor contents)))
  
(defun verb-p (word-obj)
  "Returns T if word-obj is a VERB"
  (let ((pos-list (pos word-obj)))
    (loop
	 for pos in pos-list
	 do (if (equal pos :verb)
		(return-from verb-p t)))))
 
(defun noun-p (word-obj)
  "Returns T if WORD-OBJ is a NOUN"
  (if word-obj
      (let ((pos-list (pos word-obj)))
	(loop
	   for pos in pos-list
	   do (if (equal pos :noun)
		  (return-from noun-p t))))))

(defun obj-list->tree (obj-list)
  "Takes an OBJ-LIST and returns a parsed TREE"
  )

(defun obj-list->basic-ast (obj-list)
  "Takes an OBJ-LIST and returns an abstract syntax tree. Left-child is the verb,
right-child, if-exists, is a direct object/parameter to the verb."
  (make-instance 'parse-tree-node 
		 :lc (make-instance 'parse-tree-node :contents (first obj-list)) 
		 :rc (make-instance 'parse-tree-node :contents (second obj-list))
		 :contents 'verb-phrase))

;; Binder
;; ------
;
(defun noun->obj (noun)
  "Takes a NOUN object and returns the OBJECT it refers to."
  (loop
       for object in *objects*
       do (if (string-equal (word noun) (string-downcase (name object)))
	      (return-from noun->obj object))))

(defun verb->function (verb) ;;NOTE: Only accepts directions right now
  "Takes a VERB object and returns the FUNCTION the verb is supposed to call"
  (loop
     for direction in *directions*
     do (if (string-equal (word verb) direction) 
	    (return-from verb->function (list #'move *current-player* direction)))))

(defun parse-tree->sexp (tree) ;; This is really basic!
  "Takes a parsed TREE of tokens and returns a runnable S-EXP"
  (let ((verb (contents (left-child tree))) (noun (contents (right-child tree))))
    (cond ((and (verb-p verb) (not (noun-p noun))) (verb->function verb))
	  ((and (verb-p verb) (noun-p noun)) (append (verb->function verb) (noun->obj noun)))
	  (t nil))))
	
(defun string->sexp (string) ;; uses the basic abstract-syntax tree!!
  "Takes a STRING and turns it into a valid S-EXP to run."
  (parse-tree->sexp
   (obj-list->basic-ast
    (string->obj-list string))))

;;Execution example
;; (apply #'funcall (string->sexp (user-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;~~~~~~~~~~~~~~~~~ Load/Save ~~~~~~~~~~~~~~~~~~;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;; (defmethod save-object ((player <player>)) ;; Currently clobbers whatever file was there before.
;;   "Saves PLAYER to the appropriate file."
;;   (cl-store:store player (merge-pathnames 
;; 			(format nil "players/player-~a.noob" (player-id player)) *game-directory*)))

;; (defun load-player (player-id)
;;   "If the player is not already loaded, and a file for them exists, load the appropriate player by PLAYER-ID"
;;   (let ((file (player-file-exists-p player-id)))
;;     (if (player-loaded-p player-id) 
;; 	(format t "Player #~a Exists, aborting load." player-id)
;; 	(if file 
;; 	    (pushnew 
;; 	     (cl-store:restore 
;; 	      (merge-pathnames 
;; 	       (format nil "players/player-~a.noob" player-id) *game-directory*)) 
;; 	     *players*)
;; 	    (format t "No file for Player #~a" player-id)))))

;; (defun player-loaded-p (player-id)
;;   "Checks to see if a player with PLAYER-ID exists in *players* list"
;;   (loop for player in *players*
;;      do (if (= (player-id player) player-id) 
;; 	    (return t)
;; 	    (return nil))))

;; (defun player-file-exists-p (player-id)
;;   "Returns a PATHNAME if a player file with PLAYER-ID exists"
;;   (probe-file (merge-pathnames (format nil "players/player-~a.noob" player-id) *game-directory*)))

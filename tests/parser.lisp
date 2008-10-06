;; Copyright 2008 Rudolf Olah

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

(in-package #:sykosomatic)

(def-suite parser)

(def-suite pre-processing-tests :in parser)
(in-suite pre-processing-tests)

(test preprocess-string
      "Tests a function that removes trailing whitespaces."
      (is (equal "hello" (preprocess-string "   hello   ")))
      (is (equal "world" (preprocess-string "   world")))
      (is (equal "world" (preprocess-string "world   ")))
      (is (equal "wow" (preprocess-string "wow")))
      (is (equal "hello world" (preprocess-string "    hello world    ")))
      (is (equal "hello    world" (preprocess-string "    hello    world    ")))
      )

(def-suite tokenizer-tests :in parser)
(in-suite tokenizer-tests)

(test split-command-string
      (is (equal '("hello" "world") (split-command-string "hello world")))
      (is (equal '("hello" "world") (split-command-string "hello    world")))
      (is (equal '("one") (split-command-string "one")))
      (is (equal '("001" "233") (split-command-string "001 233")))
      )

(test split-off-chat-string
      (is (equal '("hello" "world'") (split-off-chat-string "hello 'world'")))
      (is (equal '("hello" "this is a message'")
		 (split-off-chat-string "hello 'this is a message'")))
      (is (equal '("hello" "this is a message")
		 (split-off-chat-string "hello 'this is a message")))
      )

(test format-chat-string
      (is (equal "'hello" (format-chat-string "hello")))
      (is (equal "'hello'" (format-chat-string "hello'")))
      (is (equal "'this is a message" (format-chat-string "this is a message")))
      (is (equal "'this is a message'" (format-chat-string "this is a message'")))
      )

(test string->token-list
  (is (equal '("walk" "north") (string->token-list "walk north")))
  (is (equal '("walk" "north" "'Cool'") (string->token-list "walk north 'Cool'")))
  )

(def-suite parser-tests :in parser)
(in-suite parser-tests)

(test parse-string
  ;; basic use
  (is (equal (verb (parse-string "get the flask"))
	     "get"))
  (is (equal (noun (car (direct-objects (noun-clause (parse-string "get the flask")))))
	     "flask"))
  (is (equal (car (adjectives (car (direct-objects (noun-clause (parse-string "get the flask"))))))
	     "the"))
  ;; Preposition tests
  (is (equal (verb (parse-string "get the flask with my bar"))
	     "get"))
  (is (equal (prepositions (noun-clause (parse-string "get the flask with my bar")))
	     '(NIL "with")))
  (is (equal (prepositions (noun-clause (parse-string "get at the flask with my bar")))
	     '("at" "with")))
  (is (equal (adjectives (car (direct-objects (noun-clause (parse-string "get the at flask with bar")))))
	     '("at" "the")))
  (signals parser-error (parse-string "get at the flask with with my bar"))
  (signals parser-error (parse-string "get the flask on the table with my hands"))
  (signals parser-error (parse-string "get the flask with my bar with"))
  ;; possessives
  (is (equal 
       (noun
	(owns
	 (owns
	  (owns
	   (owns
	    (owns 
	     (car (direct-objects 
		   (noun-clause (parse-string "get my shoe's laces' color's tint's level"))))))))))
       "level"))
  (is (equal 
       (noun (car (direct-objects (noun-clause (parse-string "get my green handsome shoe's shoelaces from my pouch")))))
       "my"))
  ;; conjunctions
  (is (equal
       (length (direct-objects (noun-clause (parse-string "get my flask and my hat and my fork and my spoon and the orange thing with my hands and my feet"))))
       5))
  (is (equal
       (length (indirect-objects (noun-clause (parse-string "get my flask and my hat and my fork and my spoon and the orange thing with my hands and my feet"))))
       2)))




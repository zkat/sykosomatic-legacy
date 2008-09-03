;; -*- common-lisp -*-

(in-package #:sykosomatic-tests)

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
      (is (equal '("hello" "world") (split-off-chat-string "hello 'world'")))
      (is (equal '("hello" "this is a message")
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
      (is (equal '("walk" "north") (string->token-list "walk north 'Cool'")))
      )

(def-suite parser-tests :in parser)
(in-suite parser-tests)

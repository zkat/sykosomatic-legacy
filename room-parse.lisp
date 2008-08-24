;; room-parse.lisp
;; Room Parser for sykosomatic
;; Meh, coded by Rudolf Olah <omouse@gmail.com>
;; A conversion of jeebusroxors's room-parse.py code

#| Input in XML:
    <roomlist>
      <room>
        <name>This in the name</name>
        <desc>description</desc>
        <long-desc>long description</long-desc>
      </room>
    </roomlist>
   Output to Lisp:
    (make-room :name "This is the name"
               :desc "desc"
               :desc-long "desc-long"
    )
|#

(require 'asdf)
(require 'xmls)

(defconstant +required-room-elements+ '("name" "desc" "desc-long"))

(defun load-roomlist (filename)
  (with-open-file (s filename :direction :input)
		  (xmls:parse s)))

(defun save-room (room-node stream)
  (let ((children (cddr room-node)))
    (if (subsetp +required-room-elements+ (mapcar #'car children) :test #'equal)
	(format stream "(make-room ~:{~12T:~A ~*~S~%~})~%" children)
	(error "The room contains: ~S, but is missing one of these required elements: ~S"
	       children +required-room-elements+))))

(defun save-roomlist (roomlist-xml-node stream)
  (mapc (lambda (room) (save-room room stream))
	(cddr roomlist-xml-node)))

(defun convert-xml-roomlist (from to)
  (with-open-file (s to :direction :output :if-exists :supersede)
		  (save-roomlist (load-roomlist from) s)))

;; Test: (convert-xml-roomlist "roomlist.xml" "room-parse-output")
(in-package :sykosomatic.socket-server)

;;;
;;; Queue util
;;;
(defun make-queue ()
  (cons nil nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
            (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun queue-empty-p (queue)
  (null (car queue)))

(defun queue-peek (q)
  (caar q))

(defun queue-tail (q)
  (car (last (cdr q))))

;;;
;;; Strings
;;;
(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun string-cleanup (string)
  (remove-if-not (lambda (char)
                   (or (graphic-char-p char)
                       (whitespacep char)))
                 (string-trim '(#\Space #\Tab #\Newline #\Return)
                              string)))

;;;
;;; Utility macros
;;;
(defmacro defaccessor (name &optional (var-name 'x))
  `(progn
     (defgeneric ,name (,var-name))
     (defgeneric (setf ,name) (new-value ,var-name))))

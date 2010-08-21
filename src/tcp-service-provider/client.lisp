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

;; client.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(declaim (optimize debug))
;;;
;;; TCP Client class
;;;
(defparameter *default-client-main* nil)

(defclass tcp-client (client fundamental-character-output-stream
                             fundamental-character-input-stream)
  ((socket :accessor socket :initarg :socket
           :initform (error "Must provide a socket for this client."))
   (remote-name :accessor remote-name)
   (port :accessor port)
   (max-buffer-bytes :reader max-buffer-bytes :initarg :max-buffer-bytes :initform 16384)
   (input-buffer :accessor input-buffer)
   (input-buffer-fill :initform 0 :accessor input-buffer-fill)
   (output-buffer-queue :accessor output-buffer-queue :initform (make-queue))
   (output-buffer :accessor output-buffer :initform nil)
   (output-byte-count :accessor output-byte-count :initform 0)
   (recent-newline-p :accessor recent-newline-p :initform t)
   (last-input :accessor last-input :initform nil)
   (client-main :accessor client-main :initarg :client-main :initform *default-client-main*)
   (client-continuation :accessor client-continuation :initform nil)))

(defmethod initialize-instance :after ((client tcp-client) &key)
  (multiple-value-bind (name port)
      (iolib:remote-name (socket client))
    (setf (remote-name client) name
          (port client) port
          (input-buffer client) (make-array (max-buffer-bytes client)
                                            :element-type '(unsigned-byte 8)))))

(defmethod print-object ((client tcp-client) s)
  (print-unreadable-object (client s :type t)
    (format s "~A:~A" (remote-name client) (port client))))

;;;
;;; Gray streams stuff
;;;

;;; Basic
(defmethod close ((client tcp-client) &key abort)
  (close (socket client) :abort abort))
(defmethod open-stream-p ((client tcp-client))
  (open-stream-p (socket client)))

;; Input
(defmethod stream-read-line ((client tcp-client))
  (read-line-from-client client))
(defmethod stream-clear-input ((client tcp-client))
  (setf (input-buffer-fill client) 0))

;; Output
(defmethod stream-write-char ((client tcp-client) char)
  (write-to-client client (princ-to-string char))
  (setf (recent-newline-p client)
        (eq #\newline char))
  char)
(defmethod stream-line-column ((client tcp-client))
  nil)
(defmethod stream-start-line-p ((client tcp-client))
  (when (recent-newline-p client)
    t))
(defmethod stream-fresh-line ((client tcp-client))
  (unless (stream-start-line-p client)
    (stream-write-char client #\newline)))
(defmethod stream-write-string ((client tcp-client) seq &optional start end)
  (let ((seq (if start (subseq seq start end) seq)))
    (when (plusp (length seq))
      (prog1 (write-to-client client seq)
        (setf (recent-newline-p client)
              (when (eql #\newline (elt seq (1- (length seq)))) t))))))
(defmethod stream-clear-output ((client tcp-client))
  (setf (output-buffer client) nil
        (output-byte-count client) 0))

;;;
;;; Connection
;;;
(defgeneric disconnect (client &rest events)
  (:method ((client tcp-client) &rest events)
    (let ((fd (iolib:socket-os-fd (socket client)))
          (event-base (event-base (service-provider client))))
      (if (not (intersection '(:read :write :error) events))
          (iolib:remove-fd-handlers event-base fd
                                    :read t :write t :error t)
          (progn
            (when (member :read events)
              (iolib:remove-fd-handlers event-base fd :read t))
            (when (member :write events)
              (iolib:remove-fd-handlers event-base fd :write t))
            (when (member :error events)
              (iolib:remove-fd-handlers event-base fd :error t)))))
    (when (member :close events)
      (teardown client))))

;;;
;;; Input
;;;
(defun ensure-simple-string (string)
  (cond ((simple-string-p string)
         string)
        ((stringp string)
         (aprog1 (make-string (length string) :element-type 'character)
           (map-into it #'identity string)))
        (t (error "Not a string: ~A" string))))

(defgeneric read-line-from-client (client)
  (:method ((client tcp-client))
    (let ((buffer (input-buffer client))
          (buffer-fill (input-buffer-fill client)))
      (when (plusp buffer-fill)
        ;; We've got input! Try to convert it to a string.
        (flex:with-input-from-sequence (s buffer :end buffer-fill)
          (setf s (flex:make-flexi-stream s :external-format :utf-8))
          (aprog1 (loop with string = (make-array 16 :element-type 'character
                                                  :adjustable t :fill-pointer 0)
                     for char = (handler-case (read-char s nil nil)
                                  (flex:external-format-encoding-error ()
                                    (read-byte s nil nil)))
                     do (cond ((and (characterp char) (not (char= #\newline char)))
                               (vector-push-extend char string))
                              ((and (or (null char)
                                        (char= #\newline)))
                               (return (ensure-simple-string string)))
                              ((numberp char)
                               ;; Ignore special codes for now
                               ;; TODO - If a special code is received, this should
                               ;;        trigger a telnet-code-handler callback.
                               nil)
                              (t
                               (return nil))))
            (if (flex:peek-byte s nil nil nil)
                ;; If there's still stuff in the buffer, we'll have to shift things to the left.
                (loop
                   for next-byte = (read-byte s nil nil)
                   for i from 0
                   while next-byte
                   do (setf (aref buffer i) next-byte)
                   finally (setf (input-buffer-fill client) (1+ i)))
                (setf (input-buffer-fill client) 0))))))))

(defgeneric handle-line (client line)
  (:method ((client tcp-client) line)
    (format t "~&~A sez: ~S~%" client line)
    (setf (last-input client) line)
    (update client)))

(defgeneric on-client-read (client)
  (:method ((client tcp-client))
    (handler-case
        (let ((bytes-read
               (nth-value 1 (iolib:receive-from (socket client)
                                                :buffer (input-buffer client)
                                                :start (input-buffer-fill client)
                                                :end (1- (max-buffer-bytes client))))))
          (when (zerop bytes-read)
            (error 'end-of-file))
          (incf (input-buffer-fill client) bytes-read)
          (awhen (read-line client)
            (handle-line client it)))
      (iolib:socket-connection-reset-error ()
        ;; Should do something here
        (format t "Got a reset from ~A.~%" client)
        (finish-output)
        (disconnect client :close))
      (end-of-file ()
        ;; Should do something here, too.
        (format t "Received unexpected EOF from ~A.~%" client)
        (finish-output)
        (disconnect client :close)))))

(defun/cc prompt-client (client &optional format-string &rest format-args)
  (when format-string
    (apply #'format client format-string format-args))
  (string-cleanup
   (let/cc k
     (setf (client-continuation client) k))))

(defun/cc client-y-or-n-p (client format-string &rest format-args)
  (unless (string-equal "n" (apply #'prompt-client client format-string format-args))
    t))

;;;
;;; Output
;;;
(defgeneric on-client-write (client)
  (:method ((client tcp-client))
    (handler-case
        (progn
          (when (and (not (output-buffer client))
                     (not (queue-empty-p (output-buffer-queue client))))
            (setf (output-buffer client) (dequeue (output-buffer-queue client))))
          (when (output-buffer client)
            (let* ((buffer (output-buffer client))
                   (bytes-written (iolib:send-to (socket client) buffer
                                                 :start 0
                                                 :end (length buffer))))
              (incf (output-byte-count client)
                    bytes-written)
              (when (= (output-byte-count client)
                       (length buffer))
                (setf (output-buffer client) nil
                      (output-byte-count client) 0)))))
      (iolib:socket-connection-reset-error ()
        (format t "Got a reset from ~A.~%" client)
        (finish-output)
        (disconnect client :close))
      (isys:ewouldblock ()
        (format t "Got an EWOULDBLOCK while writing to ~A.~%" client)
        (finish-output))
      (isys:epipe ()
        (format t "Got a hangup while writing to ~A.~%" client)
        (disconnect client :close)))))

(defgeneric write-to-client (client data)
  (:method ((client tcp-client) (data string))
    (enqueue (flex:string-to-octets data :external-format :utf-8)
             (output-buffer-queue client))))

(defun broadcast-to-location (location format-string &rest format-args)
  (mapc (lambda (soul)
          (apply #'format (client soul) format-string format-args))
        (loop for body-id in (contents location)
           for soul = (body-id->soul body-id)
           when soul collect soul)))

(defun broadcast-to-others (body format-string &rest format-args)
  "Broadcasts output to everyone in BODY's location."
  (let ((location (make-instance 'location :document (get-document *db* (location body)))))
    (mapc (lambda (soul)
            (apply #'format (client soul) format-string format-args))
          (loop for body-id in (contents location)
             for soul = (body-id->soul body-id)
             when (and soul (not (string= body-id (uuid body))))
             collect soul))))

(defun broadcast-to-provider (provider format-string &rest format-args)
  (maphash (lambda (k client)
             (declare (ignore k))
             (apply #'format client format-string format-args))
           (clients provider)))

;;;
;;; Init/Update/Teardown
;;;
(defmethod init ((client tcp-client))
  (format client "~&Hello. Welcome to Sykosomatic.~%")
  (maybe-login client))

(defmethod update ((client tcp-client))
  (aif (client-continuation client)
       (progn (setf (client-continuation client) nil)
              (funcall it (last-input client)))
       (funcall (client-main client) client)))

(defmethod teardown ((client tcp-client))
  (close client :abort t)
  (format t "~&~A Disconnected.~%" client)
  (detach-client (service-provider client) client)
  (awhen (soul client)
    (teardown it))
  client)

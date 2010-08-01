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

;; tcp-service-provider.lisp
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

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
;;; Config variables
;;;
(defparameter *server-listen-ip* iolib:+ipv4-unspecified+)
(defparameter *server-port* 4000)
(defparameter *default-client-main* nil)

;;;
;;; TCP Clients
;;;
(defclass tcp-client (client fundamental-character-output-stream
                             fundamental-character-input-stream)
  (#+nil(input-handler :initarg :input-handler :initform nil :accessor input-handler)
   (socket :accessor socket :initarg :socket
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

;;; Gray streams stuff
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
    (prog1 (write-to-client client seq)
      (setf (recent-newline-p client)
            (when (eql #\newline (elt seq (1- (length seq)))) t)))))
(defmethod stream-clear-output ((client tcp-client))
  (setf (output-buffer client) nil
        (output-byte-count client) 0))

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
          (let ((maybe-line (read-line client)))
            (when maybe-line (handle-line client maybe-line))))
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
    (let ((array (make-array (length data) :element-type '(unsigned-byte 8))))
      (enqueue (map-into array #'char-code data)
               (output-buffer-queue client)))))

(defgeneric read-line-from-client (client)
  (:method ((client tcp-client))
    (let ((buffer (input-buffer client))
          (buffer-fill (input-buffer-fill client)))
      ;; TODO: This is *totally* wrong. If there's more than one newline in the buffer,
      ;; it'll grab the _entire_ thing as a single string! Use ring buffer? :\
      (when (and (plusp buffer-fill)
                 (find #.(char-code #\Newline) buffer :end buffer-fill))
        (setf (recent-newline-p client) t)
        ;; Note: We _scrap_ the newline.
        (let ((string (make-string (1- buffer-fill))))
          (loop for code across buffer
             for i below (1- buffer-fill)
             do (setf (aref string i) (code-char code)))
          (setf (input-buffer-fill client) 0)
          string)))))

(defgeneric handle-line (client line)
  (:method ((client tcp-client) line)
    (format t "~A sez: ~A~%" client line)
    (setf (last-input client) line)
    (update client)))

(defun broadcast-to-room (client format-string &rest format-args)
  (let ((text (apply #'format nil format-string format-args)))
    (maphash (lambda (k current-client)
               (declare (ignore k))
               (unless (eq client current-client)
                 (princ text current-client)))
             (clients (service-provider client)))))

(defun broadcast-to-provider (provider format-string &rest format-args)
  (let ((text (apply #'format nil format-string format-args)))
    (maphash (lambda (k client)
               (declare (ignore k))
               (princ text client))
             (clients provider))))

(defmethod init ((client tcp-client))
  (format client "~&Hello. Welcome to Sykosomatic.~%")
  (maybe-login client))

(defmethod update ((client tcp-client))
  (let ((k (client-continuation client)))
    (if k
        (progn (setf (client-continuation client) nil)
               (funcall k (last-input client)))
        (funcall (client-main client)))))

(defmethod teardown ((client tcp-client))
  (close client :abort t)
  (format t "~&~A Disconnected.~%" client)
  (detach-client (service-provider client) client)
  client)

;;;
;;; TCP service
;;;
(defclass tcp-service-provider (service-provider)
  ((event-base :initform nil :accessor event-base :initarg :event-base)
   (listen-ip :initform *server-listen-ip*  :accessor listen-ip)
   (port :initform *server-port* :accessor port :initarg :port)
   (clients :initform (make-hash-table :test 'equalp) :accessor clients)
   (socket :initform nil :accessor socket)))

(defgeneric attach-client (server client)
  (:method ((server tcp-service-provider) (client tcp-client))
    (setf (gethash `(,(remote-name client) ,(port client)) (clients server))
          client)))

(defgeneric detach-client (server client)
  (:method ((server tcp-service-provider) (client tcp-client))
    (remhash `(,(remote-name client) ,(port client)) (clients server))
    client))

(defgeneric disconnect-all-clients (server)
  (:method ((server tcp-service-provider))
    (maphash (lambda (k client) (declare (ignore k))
                     (disconnect client :close))
             (clients server))))

(defmethod init ((server tcp-service-provider))
  (setf (clients server) (make-hash-table :test 'equalp)
        (event-base server) (make-instance 'iolib:event-base))
  (let ((socket (iolib:make-socket :connect :passive
                                   :address-family :internet
                                   :type :stream
                                   :ipv6 nil)))
    (iolib:bind-address socket (listen-ip server)
                        :port (port server))
    (iolib:listen-on socket :backlog 5)
    (iolib:set-io-handler (event-base server)
                          (iolib:socket-os-fd socket)
                          :read
                          (lambda (&rest rest)
                            (declare (ignore rest))
                            (on-client-connection server)))
    (setf (socket server) socket)))

(defmethod teardown ((server tcp-service-provider))
  (with-accessors ((event-base event-base) (socket socket))
      server
    (disconnect-all-clients server)
    (when event-base
      (close event-base)
      (setf event-base nil))
    (when socket
      (close socket)
      (setf socket nil))))

(defgeneric on-client-connection (server)
  (:method ((server tcp-service-provider))
    (let ((client-socket (iolib:accept-connection (socket server))))
      (when client-socket
        (let ((client (make-instance 'tcp-client
                                     :socket client-socket
                                     :provider server)))
          (format t "~A Connected.~%" client)
          (attach-client server client)
          (iolib:set-io-handler (event-base server)
                                (iolib:socket-os-fd client-socket)
                                :read
                                (lambda (&rest rest)
                                  (declare (ignore rest))
                                  (on-client-read client)))
          (iolib:set-io-handler (event-base server)
                                (iolib:socket-os-fd client-socket)
                                :write
                                (lambda (&rest rest)
                                  (declare (ignore rest))
                                  (on-client-write client)))
          (init client))))))

(defgeneric dispatch-events (service-provider)
  (:method ((sp tcp-service-provider))
    (handler-case
        (iolib:event-dispatch (event-base sp) :timeout 0)
      (iolib:socket-connection-reset-error ()
        (format t "Unexpected connection reset.~%"))
      (iolib:hangup ()
        (format t "Unexpected hangup.~%"))
      (end-of-file ()
        (format t "Unexpected EOF.~%")))))

(defmethod update ((server tcp-service-provider))
  (dispatch-events server))

;;;
;;; Input handlers
;;;
(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Newline #\Return)))

(defun string-cleanup (string)
  (remove-if-not (lambda (char)
                   (or (graphic-char-p char)
                       (whitespacep char)))
                 (string-trim '(#\Space #\Tab #\Newline #\Return)
                              string)))

(defun/cc prompt-client (client &optional format-string &rest format-args)
  (when format-string
    (apply #'format client format-string format-args))
  (string-cleanup
   (let/cc k
     (setf (client-continuation client) k))))

(defun/cc client-y-or-n-p (client format-string &rest format-args)
  (unless (string-equal "n" (apply #'prompt-client client format-string format-args))
    t))

(defparameter *password-salt* "sh00rizs4lt1")
(defun hash-password (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array
     (concatenate 'string string *password-salt*)))))

(defvar *accounts* nil)
(defclass account ()
  ((username :accessor username :initarg :username)
   (password-hash :accessor password-hash)
   (email :accessor email :initarg :email)))
(defmethod initialize-instance :after ((account account) &key password)
  (setf (password-hash account) (hash-password password))
  (push account *accounts*))

(defun find-account (username)
  (find username *accounts* :key #'username :test #'string=))

(defun account-exists-p (username)
  (find-account username))

(defun confirm-password (account input)
  (string= (password-hash account)
           (hash-password input)))

(defun/cc maybe-login (client)
  (let* ((username (prompt-client client "~&Username: "))
         (account (find-account username)))
    (if account
        (login client account)
        (no-such-account client username))))

(defun/cc login (client account)
  (let ((account (confirm-password account (prompt-client client "~&Password: "))))
    (if account
        (progn
          (setf (soul client) (make-instance 'soul :account account :client client))
          #+nil(format client "~&You are now logged in as ~A.~%" (username (account (soul client))))
          (format client "~&Commands: 'look' and 'quit'. Type anything else to chat.~%")
          #+nil(broadcast-to-room client "~&~A enters the world.~%" (username (account (soul client)))))
        (progn
          (format client "~&Wrong password.~%")
          (maybe-login client)))))

(defun/cc no-such-account (client username)
  (if (client-y-or-n-p client "~&No such account. Create one? (Y/n) ")
      (create-account client username)
      (progn
        #+nil(format client "~&Bye!~%")
        (close client))))

(defun/cc create-account (client username)
  (let* ((username (pick-username client username))
         (password (pick-password client))
         (email (pick-email client)))
    (make-instance 'account :username username :password password :email email)
    (format client "~&Account successfully created~%")
    (maybe-login client)))

(defun/cc pick-username (client username)
  (if (client-y-or-n-p client "~&Use ~A as your username? (Y/n) " username)
      username
      (pick-username client (prompt-client client "~&Pick a new username: "))))

(defun/cc pick-password (client)
  (let* ((password (prompt-client client "~&Pick a password: "))
         (confirm (prompt-client client "~&Confirm your password: ")))
    (if (string= password confirm)
        password
        (progn
          (format client "~&Passwords did not match.~%")
          (maybe-login client)))))

(defun/cc pick-email (client)
  (prompt-client client "~&Please enter your email address: "))

(defun play-game (client)
  (handle-player-command (soul client) (last-input client)))

(setf *default-client-main* #'play-game)

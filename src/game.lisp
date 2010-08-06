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

;; game.lisp
;;
;; Implements a basic game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :sykosomatic)

(declaim (optimize debug))

;;;
;;; Test game
;;;
(defclass game (engine)
  ()
  (:default-initargs
   :providers (list (make-instance 'tcp-service-provider))))

(defun play-game (client)
  (handle-player-command (soul client) (last-input client)))

(defmethod init ((game game))
  (setf (newbie-room game) (make-instance 'room)
        (rooms game) (list (newbie-room game))))

(defmethod run :around ((game game))
  (let ((*default-client-main* #'play-game))
    (call-next-method)))

(defmethod handle-player-command ((soul soul) input &aux (input (string-cleanup input)))
  (unless (zerop (length input))
    (cond ((string-equal input "look")
           (format (client soul)
                   "You see a double rainbow all the way across the sky. So intense.~%"))
          ((string-equal input "quit")
           (broadcast-to-room (client soul)  "~&~A leaves the world~%" (name (body soul)) input)
           (disconnect (client soul) :close))
          (t
           (format (client soul) "~&You say, \"~A\"~%" input)
           (broadcast-to-room (client soul)  "~&~A says, \"~A\"~%" (name (body soul)) input)))))

(defun main ()
  (run (make-instance 'game)))

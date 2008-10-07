;; Copyright 2008 Kat Marchan

;; This file is part of sykosomatic

(defpackage #:org.sykosomatic.util
  (:use :cl :bordeaux-threads)
  (:export

   ;; logger
   :*log-directory*
   :log-message

   ;; priority queue
   :make-priority-queue
   :priority-queue-minimum
   :priority-queue-extract-minimum
   :priority-queue-insert
   :priority-queue-empty-p
   :priority-queue-remove
   
   ;; queue
   :make-empty-queue
   :queue
   :enqueue
   :dequeue
   :queue-empty-p))
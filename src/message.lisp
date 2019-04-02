(defpackage #:cl-telegram-bot/message
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/chat
                #:get-chat-id
                #:make-chat)
  (:import-from #:cl-telegram-bot/entities/core
                #:make-entity)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/pipeline
                #:process)
  (:import-from #:cl-telegram-bot/bot
                #:bot)
  (:import-from #:serapeum
                #:defvar-unbound)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call
		#:response)
  (:export
   #:make-message
   #:get-text
   #:get-raw-data
   #:message-chat
   #:get-entities
   #:message
   #:on-message
   #:reply
   #:get-current-chat))
(in-package cl-telegram-bot/message)


(defvar-unbound *current-bot*
  "An internal variable to hold current bot for replying.")

(defvar-unbound *current-message*
  "An internal variable to hold current message for replying.")


(defclass message ()
  ((text :initarg :text
         :reader get-text)
   (chat :initarg :chat
         :reader message-chat)
   (entities :initarg :entities
             :initform nil
             :reader get-entities)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))


(defun make-message (data)
  (let ((message (make-instance 'message
                                :text (getf data :|text|)
                                :chat (make-chat (getf data :|chat|))
                                :raw-data data)))
    (setf (slot-value message 'entities)
          (mapcar (lambda (item)
                    (make-entity message item))
                  (getf data :|entities|)))

    (values message)))


(defmethod print-object ((message message) stream)
  (print-unreadable-object
      (message stream :type t)
    (format stream
            "text=~A chat=~A"
            (get-text message)
            (message-chat message))))


(define-condition reply-immediately ()
  ((text :initarg :text
         :reader get-text)
   (args :initarg :args
         :reader get-rest-args)))


(defun reply (text
              &rest args
              &key
                parse-mode
                disable-web-page-preview
                disable-notification
                reply-to-message-id
                reply-markup)
  (declare (ignorable parse-mode
                      disable-web-page-preview
                      disable-notification
                      reply-to-message-id
                      reply-markup))
  "Works like a send-message, but only when an incoming message is processed.
   Automatically sends reply to a chat from where current message came from."
  (unless (and (boundp '*current-bot*)
               (boundp '*current-message*))
    (error "Seems (reply ~S) was called outside of processing pipeline, because no current message is available."
           text))

  (signal 'reply-immediately
          :text text
          :args args))


(defgeneric on-message (bot text)
  (:documentation "This method gets called with raw text from the message.
                   By default it does nothing."))


(defmethod on-message ((bot t) text)
  (declare (ignorable text))
  (log:warn "Ignoring messages's text. Define on-message method to process it.")
  (values))


(defmethod process ((bot t) (message message))
  "By default, just calls `process' on each entity. And after that calls (on-message bot text).

   This method binds its arguments to *current-bot* and *current-message*
   to make it easier to use (reply \"text\") in 99% usecases.

   If (reply \"text\") is called during processing of some entity or inside the on-message, then
   whole processing pipeline will be stopped and next update will be processed."
  (log:debug "Processing message" message)
  
  (let ((*current-bot* bot)
        (*current-message* message))
    
    (handler-case
        (progn (loop for entity in (get-entities message)
                     do (process bot entity))

               (on-message bot
                           (get-text message)))
      (reply-immediately (condition)
        (log:debug "Replying to" *current-message*)
        (apply #'send-message
               *current-bot*
               (message-chat *current-message*)
               (get-text condition)
               (get-rest-args condition)))))
  (values))


(defun get-current-chat ()
  "Returns a chat where currently processing message was received."
  (unless (boundp '*current-message*)
    (error "Seems (get-current-chat) was called outside of processing pipeline, because no current message is available."))

  (message-chat *current-message*))

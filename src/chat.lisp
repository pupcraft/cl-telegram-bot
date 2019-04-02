(defpackage #:cl-telegram-bot/chat
  (:use #:cl)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:cl-telegram-bot/telegram-call
                #:prepare-arg
                #:def-telegram-call
                #:response)
  (:import-from #:alexandria
                #:ensure-symbol)
  (:export
   #:make-chat
   #:get-raw-data
   #:get-chat-id
   #:get-username
   #:get-first-name
   #:get-last-name
   #:chat
   #:private-chat
   #:get-chat-by-id
   #:export-chat-invite-link
   #:promote-chat-member
   #:restrict-chat-member
   #:unban-chat-member
   #:kick-chat-member
   #:set-chat-title
   #:delete-chat-photo
   #:set-chat-photo
   #:set-chat-description
   #:pin-chat-message
   #:unpin-chat-message
   #:leave-chat
   #:get-chat-administrators
   #:get-chat-members-count
   #:get-chat-member
   #:send-chat-action))
(in-package cl-telegram-bot/chat)


(defclass chat ()
  ((id :initarg :id
       :reader get-chat-id)
   (raw-data :initarg :raw-data
             :reader get-raw-data)))


(defclass private-chat (chat)
  ((username :initarg :username
             :reader get-username)
   (first-name :initarg :first-name
               :reader get-first-name)
   (last-name :initarg :last-name
              :reader get-last-name)))


(defun make-chat (data)
  (unless (string-equal (getf data :|type|)
                        "private")
    (error "Only private chats are supported for now."))
  
  (make-instance 'private-chat
                 :id (getf data :|id|)
                 :username (getf data :|username|)
                 :first-name (getf data :|first_name|)
                 :last-name (getf data :|last_name|)
                 :raw-data data))


(defmethod print-object ((chat private-chat) stream)
  (print-unreadable-object
      (chat stream :type t)
    (format stream
            "id=~A username=~A"
            (get-chat-id chat)
            (get-username chat))))


(defun get-chat-by-id (bot chat-id)
  (make-chat (get-chat bot chat-id)))

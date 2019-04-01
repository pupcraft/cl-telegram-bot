(defpackage #:cl-telegram-bot/media
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/media)


;; TODO: refactor

(def-telegram-call
    send-photo (chat-id photo &key caption disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendphoto")

(def-telegram-call
    send-audio (chat-id audio &key caption duration performer title disable-notification reply-to-message-id reply-markup)
  
  "https://core.telegram.org/bots/api#sendaudio")
(def-telegram-call send-document
    (chat-id document &key caption disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#senddocument")

(def-telegram-call
    send-video (chat-id video &key duration width height caption disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendvideo")

(def-telegram-call
    send-voice (chat-id voice &key caption duration disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendvoice")

(def-telegram-call
    send-video-note (chat-id video-note &key duration length disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendvideonote")

(def-telegram-call
    send-location (chat-id latitude longitude &key disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendlocation") 

(def-telegram-call
    send-venue (chat-id latitude longitude title address &key foursquare-id disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendvenue")

(def-telegram-call
    send-contact
    (chat-id phone-number first-name &key last-name disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendcontact")



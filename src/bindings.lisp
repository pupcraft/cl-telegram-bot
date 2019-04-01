(defpackage #:cl-telegram-bot/bindings
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/bindings)

;;files
(in-package cl-telegram-bot/files)
(def-telegram-call get-file (file-id)
  "https://core.telegram.org/bots/api#getfile")


;;game
(defpackage #:cl-telegram-bot/game
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package :cl-telegram-bot/game)

(def-telegram-call send-game
    (chat-id game-short-name &key disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendgame")

(def-telegram-call set-game-score
    (user-id score &key force disable-edit-message chat-id message-id inline-message-id)
  "https://core.telegram.org/bots/api#setgamescore")

(def-telegram-call get-game-high-scores
    (user-id &key chat-id message-id inline-message-id)
  "https://core.telegram.org/bots/api#getgamehighscores")

;;inline
(defpackage #:cl-telegram-bot/inline
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/inline)

(def-telegram-call answer-inline-query
    (inline-query-id results &key cache-time is-personal next-offset switch-pm-text)
    "https://core.telegram.org/bots/api#answerinlinequery")

;;inline-keyboard
(defpackage #:cl-telegram-bot/inline-keyboard
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/inline-keyboard)

(def-telegram-call answer-callback-query (callback-query-id &key text show-alert url)
  "https://core.telegram.org/bots/api#answercallbackquery")

;;media
(defpackage #:cl-telegram-bot/media
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/media)

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

;;messages
(in-package cl-telegram-bot/message)
(def-telegram-call
    send-message (chat text &key
		       parse-mode
		       disable-web-page-preview
		       disable-notification
		       reply-to-message-id
		       reply-markup)
  "https://core.telegram.org/bots/api#sendmessage"
  (log:debug "Sending message" chat text)
  response)

;; TODO: сделать так чтобы def-telegram-call работал c 
;; (def-telegram-call send-message (chat text &key
;;                                       parse-mode
;;                                       disable-web-page-preview
;;                                       disable-notification
;;                                       reply-to-message-id)
;;   "https://core.telegram.org/bots/api#sendmessage"
;;   (log:debug "Sending message" chat text)
;;   (let ((options
;;           (append
;;            `(:|chat_id| ,(get-chat-id chat)
;;              :|text| ,text)
;;            (when parse-mode
;;              `(:|parse_mode| ,parse-mode))
;;            (when disable-web-page-preview
;;              `(:disable_web_page_preview ,disable-web-page-preview))
;;            (when disable-notification
;;              `(:disable_notification ,disable-notification))
;;            (when reply-to-message-id
;;              `(:reply_to_message_id ,reply-to-message-id)))))
;;     (make-request bot "sendMessage" options)))


;; TODO: refactor

(def-telegram-call
    forward-message (chat-id from-chat-id message-id &key disable-notification)
  "https://core.telegram.org/bots/api#forwardmessage")

(def-telegram-call
    edit-message-text (text &key chat-id message-id inline-message-id parse-mode disable-web-page-preview reply-markup)
  "https://core.telegram.org/bots/api#editmessagetext")

(def-telegram-call
    edit-message-caption (&key chat-id message-id inline-message-id caption reply-markup)
  "https://core.telegram.org/bots/api#editmessagecaption")

(def-telegram-call
    edit-message-reply-markup (&key chat-id message-id inline-message-id reply-markup)
  "https://core.telegram.org/bots/api#editmessagereplymarkup")

(def-telegram-call
     delete-message (chat-id message-id)
  "https://core.telegram.org/bots/api#deletemessage")

;;payments
(defpackage #:cl-telegram-bot/payments
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/payments)

(def-telegram-call
    send-invoice (chat-id title description payload provider-token start-parameter currency prices &key photo-url photo-size photo-width photo-height need-name need-phone-number need-email need-shipping-address is-flexible disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendinvoice")

(def-telegram-call
    answer-shipping-query (shipping-query-id ok &key shipping-options error-message)
  "https://core.telegram.org/bots/api#answershippingquery")

(def-telegram-call
    answer-pre-checkout-query (pre-checkout-query-id ok &key error-message)
  "https://core.telegram.org/bots/api#answerprecheckoutquery")


;;profile
(defpackage #:cl-telegram-bot/profile
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/profile)

(def-telegram-call
    get-user-profile-photos (user-id &key offset limit)
  "https://core.telegram.org/bots/api#getuserprofilephotos")

;;stickers
(defpackage #:cl-telegram-bot/stickers
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/stickers)

(def-telegram-call
    send-sticker (chat-id sticker &key disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendsticker")
(def-telegram-call
    get-sticker-set (name)
  "https://core.telegram.org/bots/api#getstickerset")

(def-telegram-call
    upload-sticker-file (user-id png-sticker)
  "https://core.telegram.org/bots/api#uploadstickerfile")

(def-telegram-call create-new-sticker-set (user-id name title png-sticker emojis &key contains-masks mask-position)
  "https://core.telegram.org/bots/api#createnewstickerset")

(def-telegram-call
    add-sticker-to-set (user-id name png-sticker emojis &key mask-position)
  "https://core.telegram.org/bots/api#addstickertoset")

(def-telegram-call
    set-sticker-position-in-set (sticker position)
  "https://core.telegram.org/bots/api#setstickerpositioninset")

(def-telegram-call
    delete-sticker-from-set (sticker)
  "https://core.telegram.org/bots/api#deletestickerfromset")

;;Webhooks
(defpackage #:cl-telegram-bot/webhooks
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call
		#:response))
(in-package cl-telegram-bot/webhooks)

(def-telegram-call
    set-webhook (url &key certificate max-connections allowed-updates)
  "https://core.telegram.org/bots/api#setwebhook")
(def-telegram-call
    get-webhook-info ()
  "https://core.telegram.org/bots/api#getwebhookinfo"
  (log:debug "Retriving webhook info")
  response)


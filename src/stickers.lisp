(defpackage #:cl-telegram-bot/stickers
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/stickers)

;; TODO: refactor

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


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

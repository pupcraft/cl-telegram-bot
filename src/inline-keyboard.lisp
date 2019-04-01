(defpackage #:cl-telegram-bot/inline-keyboard
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/inline-keyboard)

;; TODO: refactor

(def-telegram-call answer-callback-query (callback-query-id &key text show-alert url)
  "https://core.telegram.org/bots/api#answercallbackquery")


(defpackage #:cl-telegram-bot/webhooks
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/webhooks)

;; TODO: refactor

(def-telegram-call
    set-webhook (url &key certificate max-connections allowed-updates)
  "https://core.telegram.org/bots/api#setwebhook")
(def-telegram-call
    get-webhook-info ()
  "https://core.telegram.org/bots/api#getwebhookinfo"
  ;;FIXME::return the webhook info
  ;;(log:debug "Retriving webhook info")
  )


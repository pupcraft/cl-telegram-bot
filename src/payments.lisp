(defpackage #:cl-telegram-bot/payments
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/payments)


;; TODO: refactor

(def-telegram-call
    send-invoice (chat-id title description payload provider-token start-parameter currency prices &key photo-url photo-size photo-width photo-height need-name need-phone-number need-email need-shipping-address is-flexible disable-notification reply-to-message-id reply-markup)
  "https://core.telegram.org/bots/api#sendinvoice")

(def-telegram-call
    answer-shipping-query (shipping-query-id ok &key shipping-options error-message)
  "https://core.telegram.org/bots/api#answershippingquery")

(def-telegram-call
    answer-pre-checkout-query (pre-checkout-query-id ok &key error-message)
  "https://core.telegram.org/bots/api#answerprecheckoutquery")

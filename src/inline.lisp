(defpackage #:cl-telegram-bot/inline
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/inline)

(def-telegram-call answer-inline-query
    (inline-query-id results &key cache-time is-personal next-offset switch-pm-text)
    "https://core.telegram.org/bots/api#answerinlinequery")

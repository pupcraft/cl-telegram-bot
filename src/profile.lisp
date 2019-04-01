(defpackage #:cl-telegram-bot/profile
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call))
(in-package cl-telegram-bot/profile)

;; TODO: refactor

(def-telegram-call
    get-user-profile-photos (user-id &key offset limit)
  "https://core.telegram.org/bots/api#getuserprofilephotos")

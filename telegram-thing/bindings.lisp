(defpackage #:cl-telegram-bot/bindings
  (:use #:cl)
  (:import-from #:cl-telegram-bot/telegram-call
                #:def-telegram-call
		#:response))
(in-package cl-telegram-bot/bindings)

(utility:eval-always
  (defparameter *functions*
    '(
      ;;chat
      (get-chat (chat-id))
      (kick-chat-member (chat-id user-id until-date))
      (unban-chat-member (chat-id user-id))
      (restrict-chat-member
       (chat-id user-id until-date can-send-messages can-send-media-messages
	can-send-other-messages can-add-web-page-previews))
      (promote-chat-member
       (chat-id user-id can-change-info can-post-messages can-edit-messages
	can-delete-messages can-invite-users can-restrict-members can-pin-messages
	can-promote-members))
      (export-chat-invite-link (chat-id))
      (set-chat-photo (chat-id photo))
      (delete-chat-photo (chat-id))
      (set-chat-title (chat-id title))
      (set-chat-description (chat-id description))
      (pin-chat-message (chat-id message-id disable-notification))
      (unpin-chat-message (chat-id))
      (leave-chat (chat-id))
      (get-chat-administrators (chat-id))
      (get-chat-members-count (chat-id))
      (get-chat-member (chat-id user-id))
      (send-chat-action (chat-id action))
      ;;files
      (get-file (file-id))
      ;;game
      (send-game
       (chat-id game-short-name &key disable-notification reply-to-message-id
	reply-markup))
      (set-game-score
       (user-id score &key force disable-edit-message chat-id message-id
	inline-message-id))
      (get-game-high-scores (user-id &key chat-id message-id inline-message-id))
      ;;inline
      (answer-inline-query
       (inline-query-id results &key cache-time is-personal next-offset switch-pm-text))
      ;;inline-keyboard
      (answer-callback-query (callback-query-id &key text show-alert url))
      ;;media
      (send-photo
       (chat-id photo &key caption disable-notification reply-to-message-id
	reply-markup))
      (send-audio
       (chat-id audio &key caption duration performer title disable-notification
	reply-to-message-id reply-markup))
      (send-document
       (chat-id document &key caption disable-notification reply-to-message-id
	reply-markup))
      (send-video
       (chat-id video &key duration width height caption disable-notification
	reply-to-message-id reply-markup))
      (send-voice
       (chat-id voice &key caption duration disable-notification reply-to-message-id
	reply-markup))
      (send-video-note
       (chat-id video-note &key duration length disable-notification
	reply-to-message-id reply-markup))
      (send-location
       (chat-id latitude longitude &key disable-notification reply-to-message-id
	reply-markup))
      (send-venue
       (chat-id latitude longitude title address &key foursquare-id
	disable-notification reply-to-message-id reply-markup))
      (send-contact
       (chat-id phone-number first-name &key last-name disable-notification
	reply-to-message-id reply-markup))
      ;;messages
      (send-message
       (chat-id text &key parse-mode disable-web-page-preview disable-notification
	reply-to-message-id reply-markup))
      (forward-message (chat-id from-chat-id message-id &key disable-notification))
      (edit-message-text
       (text &key chat-id message-id inline-message-id parse-mode
	disable-web-page-preview reply-markup))
      (edit-message-caption
       (&key chat-id message-id inline-message-id caption reply-markup))
      (edit-message-reply-markup
       (&key chat-id message-id inline-message-id reply-markup))
      (delete-message (chat-id message-id))
      ;;payments
      (send-invoice
       (chat-id title description payload provider-token start-parameter currency
	prices &key photo-url photo-size photo-width photo-height need-name
	need-phone-number need-email need-shipping-address is-flexible
	disable-notification reply-to-message-id reply-markup))
      (answer-shipping-query
       (shipping-query-id ok &key shipping-options error-message))
      (answer-pre-checkout-query (pre-checkout-query-id ok &key error-message))
      ;;profile
      (get-user-profile-photos (user-id &key offset limit))
      ;;stickers
      (send-sticker
       (chat-id sticker &key disable-notification reply-to-message-id reply-markup))
      (get-sticker-set (name))
      (upload-sticker-file (user-id png-sticker))
      (create-new-sticker-set
       (user-id name title png-sticker emojis &key contains-masks mask-position))
      (add-sticker-to-set (user-id name png-sticker emojis &key mask-position))
      (set-sticker-position-in-set (sticker position))
      (delete-sticker-from-set (sticker))
      ;;Webhooks
      (set-webhook (url &key certificate max-connections allowed-updates))
      (get-webhook-info ())
      )))

(utility:etouq
  (cons 'progn
	(mapcar
	 (lambda (x)
	   `(cl-telegram-bot/telegram-call::def-telegram-call ,@x))
	 *functions*)))

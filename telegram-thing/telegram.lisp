;;(ql:quickload :cl-telegram-bot)
;;(ql:quickload :utility)
(defpackage #:the-bot
  (:use :cl))
(in-package #:the-bot)
(cl-telegram-bot:defbot echo-bot)
(defmethod cl-telegram-bot:on-message
    ((bot echo-bot)
     text)
  (dostuff bot text))
(defmethod cl-telegram-bot:on-command
    ((bot echo-bot)
     (command (eql :help))
     text)
  (declare (ignorable text))
  (cl-telegram-bot:reply "Just send me any text and I'll reply with the same text."))
(defmethod cl-telegram-bot:on-command
    ((bot echo-bot)
     (command (eql :start))
     text)
  (declare (ignorable text))
  (cl-telegram-bot:reply "Welcome Lisper! Have a fun, playing with cl-telegram-bot!"))

(defparameter *bot* nil)
(defun start ()
  (stop)
  (let ((bot (make-echo-bot "707420266:AAEVk0qKas542dxuYAQtiq5N9AoRipVy6LU")))
    (setf *bot* bot)
    (cl-telegram-bot:start-processing
     bot
     :debug t)))
(defun stop ()
  (unwind-protect
       (when *bot*
	 (cl-telegram-bot:stop-processing *bot*))
    (setf *bot* nil)))

;;;;FIXME::this depends on all the telegram api calls to be lowercase
(utility:eval-always
  (defparameter *lisp-to-json* (make-hash-table :test 'eq))
  (defun json-telegramify (tree)
    (labels ((rec (tree)
	       (cond ((keywordp tree)
		      (or (gethash tree *lisp-to-json*)
			  (setf (gethash tree *lisp-to-json*)
				(utility:keywordify
				 (string-downcase (symbol-name tree))))))
		     ((consp tree)
		      (cons (rec (car tree))
			    (rec (cdr tree))))
		     (t tree))))
      (rec tree)))
  (defparameter *json-to-lisp* (make-hash-table :test 'eq))
  (defun json-untelegramify (tree)
    (labels ((rec (tree)
	       (cond ((keywordp tree)
		      (or (gethash tree *json-to-lisp*)
			  (setf (gethash tree *json-to-lisp*)
				(utility:keywordify
				 (string-upcase (symbol-name tree))))))
		     ((consp tree)
		      (cons (rec (car tree))
			    (rec (cdr tree))))
		     (t tree))))
      (rec tree))))

(defparameter *testcase*
  (json-telegramify
   '(:inline_keyboard
     (((:callback_data "A1" :text "A")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B")
       (:callback_data "C1" :text "B"))
      ((:callback_data "A1" :text "Annnjlkjlkjlkjj;;;;;;;;;;;;;k")))))

  #+nil
  (jonathan:parse
   (alexandria:read-file-into-string
    (merge-pathnames
     "json.json"
     (asdf:system-source-directory :telegram-thing)))))
(defmacro j (thing)
  (json-telegramify thing))
(defmacro jget (thing data)
  `(json-get (j ,thing) ,data))

(defun dostuff (bot text)
  (cl-telegram-bot/bindings::send-message
   bot
   (cl-telegram-bot/message::get-current-chat)
   "what"
   :reply-markup
   *testcase*)
  (cl-telegram-bot:reply text))

(defun json-get (item data)
  (getf data item))

(defparameter *output* *standard-output*)
(defparameter *live-chats* nil)
(defmethod cl-telegram-bot/update::process ((bot t) (update cl-telegram-bot/update::update))
  "By default, just calls `process' on the payload."
  (log:debug "Processing update" update)
  (let* ((data (cl-telegram-bot/update:get-raw-data update))
	 (readable-data (json-untelegramify data)))
    (print readable-data *output*)
    (multiple-value-bind (chatid existsp) (mehfs '(:message :chat)
						 readable-data)
      ;;chatid is the id of the chat the update is from
      (when existsp
	;;add it to the live chats
	(pushnew chatid *live-chats* :test 'equalp)
	(cl-telegram-bot/message::delete-message
	 bot
	 (mehfs '(:message :chat :ID)
		readable-data)
	 (mehfs '(:message :message_id)
		readable-data)
	 ))))
  (let ((payload (cl-telegram-bot/update::get-payload update)))
    (cl-telegram-bot/update::process bot payload)))

(defun mehf (item place)
  (let* ((value (load-time-value (gensym)))
	 (thing (getf place item value)))
    (if (eq thing value)
	(values nil nil)
	(values thing t))))

(defun mehfs (place-list place)
  (dolist (item place-list)
    (multiple-value-bind (item existsp) (mehf item place)
      (if existsp
	  (setf place item)
	  (return-from mehfs (values nil nil)))))
  (values place t))

#+nil
(defun boo ()
  (cl-telegram-bot))

(defmethod cl-telegram-bot/update:process-updates ((bot t))
  "Starts inifinite loop to process updates using long polling."
  (loop
     ;;(print "what" *output*)
     (when (< 1 (length *live-chats*))
       (error "what the hell? why are there more than one chat?"))

     ;;;;This part initiates chats
     (dolist (chat *live-chats*)
       (let ((chat (cl-telegram-bot/chat:make-chat
		    (json-telegramify chat))))
	 (cl-telegram-bot/message::send-message
	  bot
	  chat
	  "sdff")))

     ;;;;This part responds to updates
     (loop for update in (restart-case
			     (cl-telegram-bot/update::get-updates bot
								  :timeout 10)
			   (cl-telegram-bot/update::continue-processing (&optional delay)
			     :report "Continue processing updates from Telegram"
			     (when delay
			       (sleep delay))
			     ;; Return no updates
			     (values)))
	do (restart-case
	       (cl-telegram-bot/update::process bot update)
	     (cl-telegram-bot/update::continue-processing (&optional delay)
	       :report "Continue processing updates from Telegram"
	       (when delay
		 (sleep delay)))))))

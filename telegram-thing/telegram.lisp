;;(ql:quickload :cl-telegram-bot)
;;(ql:quickload :utility)
(defpackage #:the-bot
  (:use :cl))
(in-package #:the-bot)
(cl-telegram-bot::defbot echo-bot)

(Setf lparallel:*kernel* (lparallel:make-kernel 2))
(defparameter *channel* (lparallel:make-channel))

(defparameter *stop* nil)

(defun start (token)
  (setf *stop* nil)
  (let ((bot (make-echo-bot token)))
    (let ((submitted 0))
      (loop
	 (multiple-value-bind (value existsp)
	     (lparallel:try-receive-result *channel*)
	   (declare (ignorable value))
	   (when existsp
	     (decf submitted))
	   (when (zerop submitted)
	     (incf submitted)
	     (lparallel:submit-task
	      *channel*
	      (lambda ()
		(one-process-iteration bot)))))
	 (when *stop* (return))))))
(defun stop ()
  (setf *stop* t))

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
  #+nil
  (cl-telegram-bot/bindings::send-message
   bot
   (cl-telegram-bot/message::get-current-chat)
   "what"
   :reply-markup
   *testcase*)
  #+nil
  (cl-telegram-bot:reply text))

(defparameter *output* *standard-output*)
(defparameter *live-chats* nil)
(defun process-one-update (bot update)
  "By default, just calls `process' on the payload."
  (log:debug "Processing update" update)
  (let* ((readable-data (json-untelegramify update)))
    (print readable-data *output*)
    (multiple-value-bind (chatid existsp) (mehfs '("message" "chat")
						 readable-data)
      ;;chatid is the id of the chat the update is from
      (when existsp
	;;add it to the live chats
	(pushnew chatid *live-chats* :test 'equalp)
	(cl-telegram-bot/bindings::delete-message
	 bot
	 (mehfs '("message" "chat" "id")
		readable-data)
	 (mehfs '("message" "message_id")
		readable-data)
	 )))))

(defun mehf (item object)
  (handler-bind
      ((simple-error
	(lambda (c)
	  (declare (ignorable c))
	  (return-from mehf (values nil nil)))))
    (values
     (jsown:val object item)
     t)))

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
(defun one-process-iteration (bot)
  ;;(print "what" *output*)
  (when (< 1 (length *live-chats*))
    (error "what the hell? why are there more than one chat?"))

     ;;;;This part initiates chats
  (dolist (chat *live-chats*)
    (cl-telegram-bot/bindings::send-message
     bot
     (jsown:val chat "id")
     "sdff"))

     ;;;;This part responds to updates
  (dolist (update (cl-telegram-bot::get-updates bot
						:timeout 10))
    (process-one-update bot update)))

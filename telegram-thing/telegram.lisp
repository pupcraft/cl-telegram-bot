;;(ql:quickload :cl-telegram-bot)
;;(ql:quickload :utility)
(defpackage #:the-bot
  (:use :cl))
(in-package #:the-bot)
(cl-telegram-bot::defbot gun-bot)

(Setf lparallel:*kernel* (lparallel:make-kernel 2))
(defparameter *channel* (lparallel:make-channel))

(defparameter *stop* t)

(defun start (token)
  (setf *stop* nil)
  (let ((bot (make-gun-bot token)))
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


(progn
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
    ;;navivate the json object as if it were a tree/directory
    (dolist (item place-list)
      (multiple-value-bind (item existsp) (mehf item place)
	(if existsp
	    (setf place item)
	    (return-from mehfs (values nil nil)))))
    (values place t)))


(defparameter *testcase*
  #+nil
  (:OBJ
   ("inline_keyboard"
    ((:OBJ ("text" . "A") ("callback_data" . "A1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1"))
     (:OBJ ("text" . "B") ("callback_data" . "C1")))
    ((:OBJ ("text" . "Annnjlkjlkjlkjj;;;;;;;;;;;;;k") ("callback_data" . "A1")))))

 
  (jonathan:parse
   (alexandria:read-file-into-string
    (merge-pathnames
     "json.json"
     (asdf:system-source-directory :cl-telegram-bot)))
   :as :jsown))

(defparameter *output* *standard-output*)
(defparameter *live-chats* nil)
(defparameter *chat-id* nil)

(defun one-process-iteration (bot)
  (cl-telegram-bot::with-locked-bot (bot)
    ;;(print "what" *output*)
    (when (< 1 (length *live-chats*))
      (error "what the hell? why are there more than one chat?"))

     ;;;;This part initiates chats
    (dolist (chat *live-chats*)
      (cl-telegram-bot/bindings::send-message
       bot
       (jsown:val chat "id")
       "sdff"))

    (when *chat-id*
      (cl-telegram-bot/bindings::send-message
       bot
       *chat-id*
       "what"
       :reply-markup
       *testcase*))
    
     ;;;;This part responds to updates
    (print 3434)
    (dolist (update (cl-telegram-bot::get-updates bot
						  :timeout 10))
      
      (multiple-value-bind (chatid existsp) (mehfs '("message" "chat")
						   update)
	(print update)
	;;chatid is the id of the chat the update is from
	(when existsp
	  ;;add it to the live chats
	  (pushnew chatid *live-chats* :test 'equalp)
	  (setf *chat-id*
		(mehfs '("message" "chat" "id")
		       update))
	  (cl-telegram-bot/bindings::delete-message
	   bot
	   (mehfs '("message" "chat" "id")
		  update)
	   (mehfs '("message" "message_id")
		  update)
	   ))))))


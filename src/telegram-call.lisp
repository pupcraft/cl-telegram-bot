(defpackage #:cl-telegram-bot/telegram-call
  (:use #:cl)
  (:import-from #:cl-arrows
                #:->)
  (:import-from #:cl-telegram-bot/utils
                #:make-json-keyword)
  (:import-from #:cl-telegram-bot/network
                #:make-request)
  (:import-from #:alexandria
                #:with-gensyms
                #:ensure-symbol
                #:make-keyword)
  (:import-from #:kebab
                #:to-camel-case)
  (:export
   #:response))
(in-package cl-telegram-bot/telegram-call)

(defun get-method-name (name)
  "Returns a name for Telegram method.
   It is a camelcased string.
   As input, receives a symbol"
  (typecase name
    ;; If it is a symbol, we need to create a camel-cased string from it
    (symbol (-> name
                (symbol-name)
                (to-camel-case)))
    (t (error "~S should be a symbol"
              name))))


(defun get-func-name (name)
  "Returns a name for the Lisp function to call a Telegram's method."
  (typecase name
    ;; If it is a symbol, return it as is.
    (symbol name)
    ;; If it is a list, then just return the first item, because it denotes
    ;; a Telegram's method name.
    (list (unless (= (length name)
                     2)
            (error "~S should be a symbol or a list of two items"
                   name))
     (let ((first-item (first name)))
       (check-type first-item symbol)
       (values first-item)))
    (t (error "~S should be a symbol or a list of two items"
              name))))


(defun parse-def-telegram-call (&optional (lambda-list '(a b &key bar)))
  (multiple-value-bind (regular ignore0 ignore1 keys)
      (alexandria:parse-ordinary-lambda-list lambda-list :normalize-keyword nil)
    (declare (ignore ignore0 ignore1))
    (values regular keys)))

(defun translate-to-telegram-api-doc (symbol)
  (concatenate 'string
	       "https://core.telegram.org/bots/api#"
	       (remove #\- (string-downcase (symbol-name symbol)))))

(defun prepare-arg (arg)
  `(,(make-json-keyword arg)
    ;; We need to intern symbol into the package which calls our macro
     ,(ensure-symbol arg)))

(defmacro def-telegram-call (name args)
  "During the body evaluaction, result of call to API will be available
   as `response'"
  (with-gensyms (opts-var bot-var)
    (let* ((func-name (get-func-name name))
           (telegram-method-name (get-method-name name)))
      ;;separate ARGS into mandatory and optional parameters. optional parameters become keywords
      (multiple-value-bind (mandatory-args optional-args) (parse-def-telegram-call args)
	(let ((mandatory-prepared-args
	       (mapcar
		(lambda (arg)
		  (destructuring-bind (key value) (prepare-arg (make-keyword arg))
		    `(add-thing ,key ,value)))
		mandatory-args)))

	  (let* (;;keyword arguments with "-supplied-p" attached
		 (optional-supplied-p-args
		  (mapcar
		   (lambda (arg)
		     (alexandria:symbolicate arg "-SUPPLIED-P"))
		   optional-args))
		 (;;for the lambda-list
		  optional-args-keywords
		  (mapcar
		   (lambda (arg supplied-p-arg)
		     `(,arg nil ,supplied-p-arg))
		   optional-args
		   optional-supplied-p-args))
		 (;;for building the json list
		  optional-prepared-args
		  (mapcar
		   (lambda (arg supplied-p-arg)
		     `(when ,supplied-p-arg
			,(cons 'add-thing (prepare-arg (make-keyword arg)))))
		   optional-args
		   optional-supplied-p-args)))	    
	    
	    `(defun ,func-name (,bot-var ,@mandatory-args
				,@(when optional-args-keywords
				    `(&key ,@optional-args-keywords)))
	       ,(translate-to-telegram-api-doc name)
	       (let (,opts-var)
		 (flet ((add-thing (key value)
			  (push value ,opts-var)
			  (push key ,opts-var)))
		   (declare (ignorable (function add-thing)))
		   ,@mandatory-prepared-args
		   ,@optional-prepared-args)
		 (let ((response (make-request ,bot-var
						,telegram-method-name
						,opts-var)))
		   (declare (ignorable response))
		   response)))))))))

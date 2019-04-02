(defsystem telegram-thing
  :depends-on ("plump"
	       "utility"
	       "alexandria"
	       "kebab"
	       "cl-arrows"
	       "jonathan"
	       "dexador"
	       "log4cl"
	       "trivial-backtrace"
	       "cl-strings")
  :serial t
  :components
  ((:module "telegram-thing"
	    :serial t
	    :components
	    ((:file "big")
	     (:file "telegram-call")
	     (:file "bindings")
	     (:file "telegram")
	     (:file "telegram-thing")))))

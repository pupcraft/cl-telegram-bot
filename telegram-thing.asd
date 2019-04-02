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
	    ((:file "pipeline")
	     (:file "bot")
	     (:file "utils")
	     (:file "network")
	     (:file "update")
	     (:file "core")
	     (:file "telegram")
	     (:file "telegram-call")
	     (:file "files")
	     (:file "bindings")
	     (:file "telegram-thing")))))

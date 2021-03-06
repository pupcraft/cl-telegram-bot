(defsystem :cl-telegram-bot
  :depends-on ("plump"
	       "utility"
	       "alexandria"
	       "kebab"
	       "cl-arrows"
	       "jonathan"
	       "dexador"
	       "log4cl"
	       "trivial-backtrace"
	       "cl-strings"
	       "fps-independent-timestep"
	       "jsown"
	       "cl-ppcre"
	       "lparallel"
	       "local-time")
  :serial t
  :components
  ((:module "telegram-thing"
	    :serial t
	    :components
	    ((:file "big")
	     (:file "bindings")
	     (:file "telegram")
	     (:file "telegram-thing")))))

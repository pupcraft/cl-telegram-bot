(asdf:defsystem #:telegram-thing
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
	       "jsown"
	       "cl-ppcre"
	       "lparallel")
  :serial t
  :components
  ((:module "telegram-thing"
	    :serial t
	    :components
	    ((:file "big")
	     (:file "bindings")
	     (:file "telegram")
	     (:file "telegram-thing")))))

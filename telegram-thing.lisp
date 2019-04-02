(defpackage #:telegram-thing
  (:use :cl))
(in-package :telegram-thing)

(defparameter *this-directory* (asdf:system-source-directory :cl-telegram-bot))
(defparameter *html-file-path*
  (merge-pathnames "Telegram_Bot_API.html"
		   *this-directory*))

(defparameter *file* (alexandria:read-file-into-string *html-file-path*))
(defparameter *root* (plump:parse *file*))

(defparameter *data0*
  (plump:get-element-by-id *root* "dev_page_content"))
#+nil
(ql:quickload :plump)

;;find h4 tags in *data0*

(defparameter *children-elements* (plump:child-elements *data0*))

(defun tag-type= (string node)
  (and
   ;(typep node 'plump:node)
)
  (string= string (plump:tag-name node)))

(defun separate-h4s (&optional (seq *children-elements*))
  (let ((acc)
	(top))
    (flet ((new (item)
	     (push item top))
	   (new-stack ()
	     (push (nreverse top) acc)
	     (setf top nil)))
      (loop for node across seq
	 do (progn
	      (when (tag-type= "h4" node)
		(new-stack))
	      (new node))))
    (nreverse acc)))
(defparameter *data1* (separate-h4s))
(defparameter *data2* (nthcdr 4 *data1*))
(defun nothing ()
  (mapcar
   (lambda (list0)
     (mapcar (lambda (node)
	       (cond ((tag-type= "h4" node)
		      (progn
			(plump:text
			 (find-if
			  (lambda (x)
			    (plump:text-node-p x))
			  (plump:children node)))))
		     ((tag-type= "p" node)
		      (plump:text
		       node))
		     (t node)))
	     (remove-if
	      (lambda (x)
		(tag-type= "blockquote" x))
	      list0)))
   *data2*))
(defparameter *data3* (nothing))

;;functions are likeThis and types are LikeThis
(defun remove-not-functions-or-objects (&optional (list *data3*))
  (remove-if
   (lambda (x)
     (find #\Space (first x)))
   list))

(defparameter *data4* (remove-not-functions-or-objects))


#+nil
(progn
  ;;remove things that are not strings or tables
  (defun remove-other-things (&optional (list *data4*))
    (mapcar
     (lambda (x)
       (remove-if-not
	(lambda (x) (or (stringp x)
			(tag-type= "table" x)))
	x))
     list))

  (defparameter *data5* (remove-other-things)))

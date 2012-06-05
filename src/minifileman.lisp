(in-package #:minifileman)

(defun pretty-directory (dir)
  (true-pathname-form (expand-pathname dir)))

(defun minifileman-list-dir (dir)
  (let ((dir (pretty-directory dir)))
    (if (directory-p dir)
      (list-directory dir)
      (error "Not a directory: ~a" dir))))

(defun quit-minifileman (&optional event)
  (declare (ignore event))
  (write-config)
  (setf *exit-mainloop* t))

(define-gui-class panel ()
  ((panel-frame 'frame
		:grid :supplied
		:column-configure '((0 :weight 1))
		:row-configure '((1 :weight 1))
		:bind (("Control-Q" #'quit-minifileman))
     ((path-entry 'entry
		  :grid '(0 0 :sticky "we"))
      (up-button 'button
		 :text "/\\"
;		 :padx 2
		 :width 1.5
		 :background "orange"
		 :grid '(0 1))
      (files-listbox 'scrolled-listbox
		     :grid '(1 0 :sticky "wens"))
;      (vert-scrollbar 'scrollbar
;		      :orientation "vertical"
;		      :grid '(1 1 :sticky "wns"))
;      (hor-scrollbar 'scrollbar
;		     :orientation "horizontal"
;		     :grid '(2 0 :sticky "wen"))
      (command-line-frame 'frame
			  :grid '(3 0 :columnspan 2 :sticky "we")
			  :column-configure '((1 :weight 1))
	((shell-switch 'check-button
		       :onvalue "yes"
		       :offvalue "no"
		       :grid '(0 0))
	 (command-entry 'entry ;history-entry
			:grid '(0 1 :sticky "we")))))))
  (:simple-slots
   (current-dir :initform "" :accessor current-dir)
   (current-file-list :initform nil :accessor file-list)))

(defmethod (setf current-dir) :after (new-dir (panel panel))
  (let ((path-entry (path-entry panel)))
    (setf (text path-entry) new-dir)
    (setf (cursor-index path-entry) :end)))

(defmethod (setf file-list) :after (new-list (panel panel))
  (let ((listbox (files-listbox panel)))
    (listbox-clear listbox)
    (listbox-append listbox new-list)))

(defun files-listbox-selection (panel)
  (mapcar #'(lambda (index)
	      (elt (file-list panel) index))
	  (listbox-get-selection (files-listbox panel))))

(defun go-to-dir (directory panel)
  (let ((directory (pretty-directory directory)))
    (setf (file-list panel) (minifileman-list-dir directory))
    (setf (current-dir panel) directory)
    (setf *default-pathname-defaults* directory)))

(defun go-up (panel)
  (go-to-dir (dirname (current-dir panel)) panel))

(defmethod initialize-instance :after ((panel panel) &key (path (config "default-dir")) (master *tk*) grid &allow-other-keys)
  (with-accessors ((panel-frame panel-frame)
		   (path-entry path-entry)
		   (up-button up-button)) panel
    (let ((listbox (listbox (files-listbox panel))))
      (bind path-entry "<Return>"
	    (callback (event)
	      (go-to-dir (text path-entry) panel)))
      (let ((go-up-callback (callback (&rest args) (go-up panel))))
	(setf (command up-button) go-up-callback)
	(bind panel-frame "<Alt-Key-Up>" go-up-callback) ; for some
							 ; reason BIND
							 ; doesn't
							 ; work on
							 ; frames
	(bind listbox "<BackSpace>" go-up-callback))
      (let ((enter-dir-callback (callback (event)
				  (go-to-dir (append-pathnames
					      (text path-entry)
					      (first (files-listbox-selection panel)))
					     panel))))
	(bind listbox "<Double-Button-1>" enter-dir-callback)
	(bind listbox "<Return>" enter-dir-callback)))
    (self-autoresize master grid))
  (and path (go-to-dir path panel)))

(defvar *panel* (list nil nil))

(defun minifileman ()
  (let ((*config* (make-instance 'config))
        (*default-pathname-defaults* *default-pathname-defaults*))
    (with-ltk ()
      (wm-title *tk* "minifileman-dev20120330.13.25")
      (setf (first  *panel*) (make-instance 'panel :grid '(0 0 :sticky "wens")))
      (setf (second *panel*) (make-instance 'panel :grid '(0 1 :sticky "wens"))))))

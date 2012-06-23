(in-package #:minifileman)

(defun pretty-directory (dir)
  (true-pathname-form (expand-pathname dir)))

(defun minifileman-list-dir (dir)
  (let ((dir (pretty-directory dir)))
    (if (directory-p dir)
      (list-directory-relative dir)
      (error "Not a directory: ~a" dir))))

(defun quit-minifileman (&optional event)
  (declare (ignore event))
  (write-config)
  (setf *exit-mainloop* t))

(define-gui-class panel (frame)
  ((path-entry 'entry
               :grid '(0 0 :sticky "we"))
   (up-button 'button
              :text "/\\"
;              :padx 2
              :width 1.5
              :background "orange"
              :grid '(0 1))
   (files-listbox 'scrolled-listbox
                  :grid '(1 0 :sticky "wens"))
   (command-line-frame 'frame
                       :grid '(3 0 :columnspan 2 :sticky "we")
                       :column-configure '((1 :weight 1))
     ((shell-switch 'check-button
                    :onvalue "yes"
                    :offvalue "no"
                    :grid '(0 0))
      (command-entry 'entry ;history-entry
                     :grid '(0 1 :sticky "we")))))
  (:default-initargs
   :column-configure '((0 :weight 1))
   :row-configure '((1 :weight 1)))
  (:simple-slots
   (current-dir :initform "" :accessor current-dir)
   (current-file-list :initform nil :accessor file-list)))

(defmethod (setf current-dir) :after (new-dir (panel panel))
  (let ((path-entry (path-entry panel)))
    (setf (text path-entry) new-dir)
    (setf (cursor-index path-entry) :end)))

(defun file-list-for-display (list)
  #+(and openmcl unix)
  (mapcar #'(lambda (x)
              (remove ccl::*pathname-escape-character* (namestring x))) ; everywhere?
          list)
  #-(and openmcl unix) list)

(defmethod (setf file-list) :after (new-list (panel panel))
  (let ((listbox (files-listbox panel)))
    (listbox-clear listbox)
    (listbox-append listbox (file-list-for-display new-list))))

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
  (bind:bind (((:accessors path-entry up-button files-listbox)
               panel)
              ((:accessors (entered-path text)) path-entry)
              ((:accessors listbox) files-listbox)
              (go-up-callback (callback (&rest args) (go-up panel)))
              (path-entry-enter-callback
               (callback (event)
                 (go-to-dir
                  (if (string-starts-with "~" entered-path)
                    (namestring (pathname-as-directory entered-path))
                    entered-path)
                  panel)))
              (enter-dir-callback
               (callback (event)
                 (go-to-dir (append-pathnames
                             (current-dir panel)
                             (first (files-listbox-selection panel)))
                            panel))))
    (bind path-entry "<Return>" path-entry-enter-callback)
    (setf (command up-button) go-up-callback)
    ;; for some reason BIND doesn't work on frames
    (bind panel "<Alt-Key-Up>" go-up-callback)
    (bind listbox "<BackSpace>" go-up-callback)
    (bind listbox "<Double-Button-1>" enter-dir-callback)
    (bind listbox "<Return>" enter-dir-callback))
  (self-autoresize master grid)
  (and path (go-to-dir path panel)))

(defvar *panel* (list nil nil))

(defun minifileman ()
  (let ((*config* (make-instance 'config))
        (*default-pathname-defaults* *default-pathname-defaults*))
    (with-ltk ()
      (wm-title *tk* "minifileman-0.1.0")
      (bind *tk* "<Control-q>" #'quit-minifileman)
      (setf (first  *panel*) (make-instance 'panel :grid '(0 0 :sticky "wens")))
      (setf (second *panel*) (make-instance 'panel :grid '(0 1 :sticky "wens"))))))

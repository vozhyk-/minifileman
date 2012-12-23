(in-package #:minifileman)

(defun pretty-directory (dir)
  (true-pathname-form (expand-pathname dir)))

(defun minifileman-list-dir (dir)
  (let ((dir (pretty-directory dir)))
    (if (directory-p dir)
      (list-directory dir :resolve-symlinks nil)
      (error "Not a directory: ~a" dir))))

(setfun by-name #'string<)
(setfun by-directory-p (less-if #'directory-p))

(defparameter *minifileman-sort-preds*
  (list #'by-directory-p #'by-name))

(defun minifileman-sort-file-list (list)
  (multi-level-sort list *minifileman-sort-preds* :key #'namestring))

(defparameter *default-panel-name* 'default)

(define-gui-class panel (frame)
  ((main-frame 'frame
               :grid '(0 1 :sticky "wens")
               :column-configure '((0 :weight 1))
               :row-configure '((1 :weight 1))
     ((path-entry 'entry
                  :grid '(0 0 :sticky "we"))
      (up-button 'button
                 :text "/\\"
;                :padx 2
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
                        :grid '(0 1 :sticky "we"))))))
   (fav-listbox 'listbox))
  (:default-initargs
   :column-configure '((0 :weight 1) (1 :weight 1) (2 :weight 1))
   :row-configure '((0 :weight 1)))
  (:simple-slots
   (current-dir :initform "" :accessor current-dir)
   (current-file-list :initform nil :accessor file-list)
   (name :initarg :name :initform *default-panel-name* :accessor name)))

(defmethod (setf current-dir) :after (new-dir (panel panel))
  (let ((path-entry (path-entry panel)))
    (setf (text path-entry) new-dir)
    (setf (cursor-index path-entry) :end)))

(defun file-list-for-display (list)
  (mapcar #'basename
          #+(and openmcl unix)
          (mapcar #'(lambda (x)
                      (remove ccl::*pathname-escape-character* (namestring x))) ; everywhere?
                  list)
          #-(and openmcl unix) list))

(defun colorize-files (panel)
  (let ((config (remove-if-not #'listp (configq file-colors)))
        (listbox (listbox (files-listbox panel))))
    (iter
      (for n :from 0)
      (for path in (file-list panel))
      (iter
        (for (pred . args) :in config)
        (when (funcall (eval pred) path)
          (apply #'listbox-configure listbox n args))))))

(defmethod (setf file-list) :around (new-list (panel panel))
  (let ((new-list (minifileman-sort-file-list new-list))
        (listbox (files-listbox panel)))
    (call-next-method new-list panel)
    (listbox-clear listbox)
    (listbox-append listbox (file-list-for-display new-list))
    (colorize-files panel)))

(defun files-listbox-selection (panel)
  (mapcar #'(lambda (index)
	      (elt (file-list panel) index))
	  (listbox-get-selection (files-listbox panel))))

(defun go-to-dir (directory panel)
  (let ((directory (pretty-directory directory)))
    (setf (file-list panel) (minifileman-list-dir directory))
    (setf (current-dir panel) directory)
    (setf *default-pathname-defaults* directory)))

(defun go-up (panel &optional from-dir)
  (go-to-dir (dirname (or from-dir (current-dir panel))) panel))

(defun last-dir (panel)
  (config `(panel ,(name panel) last-dir)))

(defun (setf last-dir) (new-dir panel)
  (setf (config `(panel ,(name panel) last-dir))
        new-dir))

(defun save-last-dir (panel)
  (setf (last-dir panel) (current-dir panel)))

(defconstant* +sep+ (cons "--------" (constantly nil)))

(defmacro fav (args &body body)
  `(lambda (&key ,@args &allow-other-keys)
     ,@body))

(defun go-to-fav (path)
  (fav (panel)
    (go-to-dir path panel)))

(defun mkfav (fav)
  (if (not (listp fav))
    (symbol-value fav)
    (cons (first fav) (eval (second fav)))))

(defun update-favs (panel)
  (bind:bind (((:accessors (listbox fav-listbox)) panel)
              (favs (mapcar #'mkfav (configq favorites))))
    (listbox-clear listbox)
    (listbox-append listbox (mapcar #'first favs))
    (bind listbox "<Double-Button-1>"
          (callback (event)
            (let-when (sel (listbox-get-selection listbox))
              (run-fav (nth (first sel) favs)
                       panel))))))

(defun run-fav (fav panel)
  (funcall (cdr fav) :panel panel))

(defmethod initialize-instance :after ((panel panel) &key path (side :left) (master *tk*) grid &allow-other-keys)
  (bind:bind (((:accessors path-entry up-button files-listbox
                           command-line-frame shell-switch command-entry
                           fav-listbox) ; all
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
                 (go-to-dir (first (files-listbox-selection panel))
                            panel))))
    (grid fav-listbox 0 (case side
                          (:left 0)
                          (:right 2))
          :sticky "wens")
    (bind path-entry "<Return>" path-entry-enter-callback)
    (setf (command up-button) go-up-callback)
    ;; temporary, will use :bind-contents when it is written
    (mult-bind `(("<Alt-Key-Up>" ,go-up-callback))
               path-entry up-button listbox command-line-frame shell-switch command-entry)
    (mult-bind `(("<BackSpace>" ,go-up-callback)
                 ("<Double-Button-1>" ,enter-dir-callback)
                 ("<Return>" ,enter-dir-callback))
               listbox))
  (self-autoresize master grid)
  (update-favs panel)
  (go-to-dir (or path (last-dir panel) (configq default-dir)) panel))

(defvar *panel* (list nil nil))

(defun quit-minifileman (&optional event)
  (declare (ignore event))
  (print 'quit) ; debug
  (save-last-dir (first  *panel*))
  (save-last-dir (second *panel*))
  (write-config)
  (setf *exit-mainloop* t))

(defun minifileman ()
  (let ((*config* (read-config))
        (*default-pathname-defaults* *default-pathname-defaults*))
    (with-ltk ()
      (wm-title *tk* "minifileman-0.1.0")
      (bind *tk* "<Destroy>" #'quit-minifileman)
      (bind *tk* "<Control-q>" #'quit-minifileman)
      (setf (first  *panel*) (make-instance 'panel :side :left  :name 1 :grid '(0 0 :sticky "wens")))
      (setf (second *panel*) (make-instance 'panel :side :right :name 2 :grid '(0 1 :sticky "wens"))))))
;;;; path-entry -- entry with up buttons

(in-package #:minifileman-user)

(define-gui-class path-entry (frame)
  ((buttons-frame 'frame
                  :grid '(0 0 :sticky "we")
                  :height 10)
   (entry 'entry
          :grid '(1 0 :sticky "we")))
  (:simple-slots
   (buttons :initform () :accessor buttons))
  (:default-initargs
    :column-configure '((0 :weight 1))))

(defun path-sep (&rest pathnames)
  (let* ((pn (aif (find-if #'identity pathnames)
              (probe-file it)
              (error "No pathname from which path-separator can be extracted")))
         (dir-pathname (pathname-as-directory pn))
         (dir-path (namestring dir-pathname)))
    (if (root-p dir-pathname)
        (elt dir-path (1- (length dir-path))) ; unportable?
        (elt dir-path
             (- (length dir-path)
                1 (length (last1 (pathname-directory dir-pathname)))
                1)))))

(defparameter *path-sep*
  (path-sep *default-pathname-defaults*
            ;; unportable?
            (user-homedir-pathname)))

(defun single-path-sep-only (positions)
  (loop
     with result = ()
     for old = nil then p
     for p in (reverse positions)
     for show = t then (> (- old p) 1)
     do (when show (push p result))
    finally (return result)))

(defun button-positions (path)
  (let ((poss (single-path-sep-only (positions *path-sep* path))))
    (if (not (directory-pathname-p path))
      (appendnew poss (list (1- (length path))))
      poss)))

(defmethod initialize-instance :after ((pe path-entry) &key master grid &allow-other-keys)
  (with-accessors ((buttons-fr buttons-frame)
                   (entry entry)
                   (buttons buttons))
      pe
    (let* ((old-value (text entry))
           (old-xview (first (xview entry)))
           (rebuild-buttons-cb
            (callback (event)
              (let ((new-value (text entry))
                    (new-xview (first (xview entry))))
                (when (or (not (equal new-value old-value))
                          (not (equal new-xview old-xview)))
                  (dolist (b buttons) (if b (destroy b)))
                  (setf buttons
                    (loop
                       with font = (cget entry :font)
                       with positions = (button-positions new-value)
                       for old = 0 then (1+ pos)
                       for pos in positions
                       for start = (- (font-measure font (subseq new-value 0 old))
                                      (* new-xview (font-measure font new-value)))
                       for width = (font-measure font (subseq new-value old (1+ pos)))
                       for count from 1
                       for button = (if (>= (+ start width) 0)
                                      (make-instance 'button
                                                     :master buttons-fr
                                                     :place `(,start 0 :width ,width :height 10)
                                                     :command (let ((count count))
                                                                (callback ()
                                                                  (setf (text entry)
                                                                    (nth-comp (- (length positions) count)
                                                                              #'dirname new-value))
                                                                  (focus entry)
                                                                  (setf (cursor-index entry) :end)))))
                       collect button))
                  (setf old-value new-value))))))
      (bind entry "<Key>" rebuild-buttons-cb)))
  (self-autoresize master grid))

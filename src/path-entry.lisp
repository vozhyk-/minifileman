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

(defun funcall-if (test fun &rest args)
  (if test
      (apply fun args)
      (first (last args))))

(defparameter *path-sep*
  (let* ((path *default-pathname-defaults*)
         (dir-pathname (pathname-as-directory path))
         (dir-path (namestring dir-pathname)))
    (if (equal (pathname-directory dir-pathname) '(:absolute))
      (elt dir-path (1- (length dir-path))) ; unportable?
      (elt dir-path
           (- (length dir-path)
              1 (length (first (last (pathname-directory dir-pathname))))
              1)))))

;;; "~" -- ?
(defun button-positions (path)
  (let ((poss (positions *path-sep* path)))
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
           (rebuild-buttons-cb ; "////"
            (callback (event)
              (let ((new-value (text entry))
                    (new-xview (first (xview entry))))
                (when (or (not (equal new-value old-value))
                          (not (equal new-xview old-xview)))
                  (dolist (b buttons) (destroy b))
                  (setf buttons
                    (loop
                       with font = (cget entry :font)
                       with positions = (button-positions new-value)
                       for old = 0 then (1+ pos)
                       for pos in positions
                       for start = (- (font-measure font (subseq new-value 0 old))
                                      (* new-xview (font-measure font new-value))) ; !!! mustn't make additional buttons below 0px
                       for width = (font-measure font (subseq new-value old (1+ pos)))
                       for count from 1
                       for button = (make-instance 'button
                                                   :master buttons-fr
                                                   :place `(,start 0 :width ,width :height 10)
                                                   :command (let ((count count))
                                                              (callback ()
                                                                (setf (text entry)
                                                                  (nth-comp (- (length positions) count)
                                                                            #'dirname new-value))
                                                                (focus entry)
                                                                (setf (cursor-index entry) :end))))
                       collect button))
                  (setf old-value new-value))))))
      (bind entry "<Key>" rebuild-buttons-cb)))
  (self-autoresize master grid))

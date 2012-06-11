(in-package #:ltk)

;; redefine
(defmethod initialize-instance :around ((w widget)
					&key
					grid row-configure column-configure
					pack
					place
                                        bind
					&allow-other-keys)
  (call-next-method)
  ;; pack widget if parameter has been supplied ; `
  (when pack                                    ; |
    (apply #'pack w pack))                      ; |
  (when place                                   ; | this is copied from ltk.lisp
    (apply #'place w place))                    ; |
  (when grid                                    ; |
    (apply #'grid w grid))                      ; '
  (dolist (b bind)
    (apply #'bind w b))
  (dolist (i row-configure)
    (apply #'grid-rowconfigure w i))
  (dolist (i column-configure)
    (apply #'grid-columnconfigure w i)))

(defmethod listbox-clear ((sl scrolled-listbox))
  (listbox-clear (listbox sl)))

(defun font-measure (font text)
  (format-wish "senddata [font measure {~/ltk::down/} {~a}]" font text)
  (read-data))
(export 'font-measure)

(defun xview (widget)
  (labels ((xview ()
             (format-wish "senddatastrings [~a xview]" (widget-path widget))
             (mapcar #'kmrcl:parse-float (read-data))))
    (do ((xv (xview) (xview)))
        ((/= (first xv) (second xv)) xv))))
(export 'xview)

(defmethod (setf cursor-index) (idx (e entry))
  (format-wish "senddata [~a icursor ~(~a~)]" (widget-path e) idx))

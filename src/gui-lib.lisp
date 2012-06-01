;;;; gui-lib.lisp -- GUI library for minifileman

(in-package #:minifileman.gui-lib)

(defun has-content-p (args)
  (oddp (length args)))

(defun content (args)
  (if (has-content-p args)
      (first (last args))
      nil))

(defun remove-content (args)
  (if (has-content-p args)
      (butlast args)
      args))

(defun strip-args (args)
  (remove-keyword-args (remove-content args) :master :grid :bind))

(defmacro slot-bind ((&key
		      ((:slot stripped-slot) (gensym) slot-supplied-p)
		      (name (gensym) name-supplied-p)
		      (type (gensym) type-supplied-p)
		      (args (gensym) args-supplied-p)
		      (whole-args (gensym) whole-args-supplied-p)
		      (content (gensym) content-supplied-p))
		     slot
		     &body body)
  `(destructuring-bind (,name ,type &rest ,whole-args) ,slot
     (declare (ignorable ,name ,type ,whole-args))
     (let* ,(nconc
	     (when slot-supplied-p `((,stripped-slot (remove-content ,slot))))
	     (when args-supplied-p
	       `((,args
		  ,(if slot-supplied-p
		       `(cddr ,stripped-slot)
		       `(cddr (remove-content ,slot))))))
	     (when content-supplied-p
	       `((,content (content ,whole-args)))))
       ,@body)))

(defmacro spec->list ((&rest slot-bind-args
		       &key
		       (name (gensym))
		       (content (gensym))
		       ((:whole-slot slot-var) (gensym))
		       ((:master master-var) (gensym) master-supplied-p)
		       (initial-master nil)
		       (nconc nil)
		       &allow-other-keys)
		      spec
		      expression)
  (let ((slot-bind-args (add-replace-keyword-args
			 (remove-keyword-args slot-bind-args :whole-slot :master :initial-master :nconc)
			 :name name
			 :content content)))
    (with-gensyms (fun-name spec-var)
      `(labels ((,fun-name (,spec-var ,master-var)
		  (declare (ignorable ,master-var))
		  (mapcan #'(lambda (,slot-var)
			      (slot-bind ,slot-bind-args ,slot-var
				(funcall ,(if nconc '#'nconc '#'cons)
					 ,expression
					 (,fun-name ,content ,name))))
			  ,spec-var)))
	 (,fun-name ,spec ,initial-master)))))

(defun spec->defclass-slots (spec)
  (spec->list (:name name) spec
    `(,name :accessor ,name)))

(defun slot->make-instance (slot master supplied-grid)
  (slot-bind (:type type :args args) slot
    (let* ((master (getf args :master master))
	   (grid (getf args :grid))
	   (grid (if (eql grid :supplied)
		     supplied-grid
		     grid)))
      `(make-instance ,type ,@(append (strip-args args) `(:master ,master) `(:grid ,grid))))))

(defun spec->binds (spec)
  (spec->list (:name name :args args :nconc t) spec
    (let ((bindings (getf args :bind)))
      (mapcar #'(lambda (binding)
		  (destructuring-bind (event fun) binding
		    `(bind ,name ,event ,fun)))
	      bindings))))

(defclass gui-class ()
  ()
  (:documentation "Superclass of all GUI classes"))

(defgeneric initialize-gui-class-instance (obj &key master grid &allow-other-keys))

(defmethod initialize-instance :after ((gc gui-class)
				       &rest rest
				       &key
				       (master *tk*)
				       grid)
  (declare (ignore master grid))
  (apply #'initialize-gui-class-instance gc rest))

(defmacro define-gui-class (name superclasses spec &rest args)
  (with-gensyms (object-var master-var grid-var)
    `(progn
       (defclass ,name ,(append-new-superclasses superclasses 'gui-class)
         ,(append
           (spec->defclass-slots spec)
           (get-alist :simple-slots args))
         ,@(awhen (get-alist :default-initargs args)
             `((:default-initargs ,@(apply #'add-replace-keyword-args '(:master *tk*) it))))
         ,@(get-alist :defclass-options args)
         ,@(assoc :documentation args))
       ,(let ((sub-of-wg (find-if (superclass-p 'widget) superclasses))) ; is a subclass of widget 
          `(defmethod initialize-gui-class-instance ((,object-var ,name)
                                                     &key
                                                     ((:master ,master-var))
                                                     ((:grid ,grid-var))
                                                     &allow-other-keys)
             (declare (ignorable ,master-var ,grid-var))
             (with-accessors ,(spec->list (:name name) spec `(,name ,name))
                 ,object-var
               ,@(spec->list (:name name :slot slot :master master
                              :initial-master (if sub-of-wg
                                                  object-var
                                                  master-var))
                   spec `(setf ,name ,(slot->make-instance slot master grid-var)))
               ,@(spec->binds spec))))
       ',name)))

(defun self-autoresize (master &optional grid)
  (let ((sticky (getf (cddr grid) :sticky)))
    (when (null grid)
      (warn "grid parameter is nil or unsupplied. ~
             Don't know how to configure master"))
    (when (and (find #\n sticky) (find #\s sticky))
      (grid-rowconfigure master (first grid) :weight 1))
    (when (and (find #\w sticky) (find #\e sticky))
      (grid-columnconfigure master (second grid) :weight 1))))

(defmacro callback (args &body body)
  `(lambda ,args
     ,@(nconc
	(let ((1st-arg (first args)))
	  (when 1st-arg
	    (if (char/= (elt (symbol-name 1st-arg) 0)
			#\&)
		`((declare (ignorable ,1st-arg)))
		`((declare (ignorable ,(second args)))))))
	body)))

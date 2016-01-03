(in-package #:cl-node)

;;; **************************************************

;;; **************************************************

#+sbcl(defmacro without-fp-traps (&body body)
	`(sb-int:with-float-traps-masked (:invalid :divide-by-zero)
	   ,@body))

#-sbcl(defmacro without-fp-traps (&body body)
	`(progn ,@body))

;;; **************************************************

;;; **************************************************


(defclass js-value-handle ()
  ((js-value :reader js-value
	     :initarg :js-value)))

(defun make-js-value-handle (js-value)
  (jx:make-persistent js-value)
  (make-instance 'js-value-handle :js-value js-value))

(defun free (js-value-handle)
  (let ((js-value (js-value js-value-handle)))
    (jx:clear-persistent js-value)
    (jx:free js-value)
    (cffi:foreign-free js-value)))

(defun convert-js-value (js-value)
  (cond 
    ((jx:is-undefined js-value)
     nil)
    ((jx:is-null js-value)
     nil)
    ((jx:is-boolean js-value) 
     (jx:get-boolean js-value))
    ((jx:is-int32 js-value)
     (cffi:convert-from-foreign (jx:get-int32 js-value) :int32))
    ((jx:is-double js-value)
     (cffi:convert-from-foreign (jx:get-double js-value) :double))
    ((jx:is-buffer js-value)
     ;; TODO
     'to-do-implement-binary-buffer)
    ((jx:is-string js-value)
     (jx:get-string js-value))
    ((jx:is-error js-value)
     (jx:get-string js-value))
    ((jx:is-function js-value)
     (make-js-value-handle js-value))
    ((jx:is-object js-value)
     (jx:get-string js-value))
    (t
     ;; TODO
     'todo-unhandled-type)))

(defun new (js-value js-type &optional value)
  (jx:new js-value)
  (ecase js-type
    (:boolean (jx:set-boolean js-value value))
    (:int32 (jx:set-int32 js-value value))
    (:double (jx:set-double js-value value))
    (:string (jx:set-string js-value value (length value)))
    (:null (jx:set-null js-value))
    (:undefined (jx:set-undefined js-value))))
  
(defmethod cffi:translate-into-foreign-memory ((value integer) (type jx:jx-value-type) js-value)
  (new js-value :int32 value))

(defmethod cffi:translate-into-foreign-memory ((value number) (type jx:jx-value-type) js-value)
  (new js-value :double (coerce value 'double-float)))

(defmethod cffi:translate-into-foreign-memory ((value null) (type jx:jx-value-type) js-value)
  (new js-value :null))

(defmethod cffi:translate-into-foreign-memory ((value string) (type jx:jx-value-type) js-value)
  (new js-value :string value))

(defmethod cffi:translate-into-foreign-memory ((js-type-and-value list) (type jx:jx-value-type) js-value)
  (apply #'new js-value js-type-and-value))

(defmethod cffi:translate-into-foreign-memory ((value (eql :undefined)) (type jx:jx-value-type) js-value)
  (new js-value :undefined))

(defmethod cffi:translate-into-foreign-memory ((value (eql t)) (type jx:jx-value-type) js-value)
  (new js-value :boolean value))

(defmethod cffi:translate-into-foreign-memory ((value js-value-handle) (type jx:jx-value-type) js-value)
  (cffi:foreign-funcall "memcpy"
			:pointer js-value
			:pointer (js-value value)
			:int (cffi:foreign-type-size '(:struct jx:jx-value))
			:void))

;;; **************************************************

;;; **************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-lambda-list (lambda-list)
    (let ((state 'start)
	  (args nil)
	  (rest nil))
      (dolist (sym lambda-list (list (nreverse args) rest))
	(ecase state
	  (start
	   (if (eql sym '&rest)
	       (setf state '&rest)
	       (push sym args)))
	  (&rest
	   (unless rest
	     (setf rest sym))))))))

(defmacro define-js-callback (name args &body body)
  (let ((js-args (gensym))
	(num-js-args (gensym)))
    (destructuring-bind (named-args rest-arg) (parse-lambda-list args)
      `(cffi:defcallback ,name :void ((,js-args :pointer) (,num-js-args :int))
	 (flet ((js-return (value)
		  (setf (cffi:mem-aref ,js-args '(:struct jx:jx-value) ,num-js-args)
			value)
		  (return-from ,name)))
	   (let (,@(loop
		      for arg in named-args
		      for i from 0
		      collect `(,arg (convert-js-value (cffi:mem-aptr ,js-args
								      '(:struct jx:jx-value)
								      ,i))))
		 ,@(when rest-arg
			 `((,rest-arg
			    (loop
			       for i from ,(length named-args) to (1- ,num-js-args)
			       collect (convert-js-value (cffi:mem-aptr ,js-args
									'(:struct jx:jx-value)
									i)))))))
	     ,@body))))))

;;; **************************************************

;;; **************************************************

(defvar *callbacks*
  (make-hash-table :test 'equal))

(defclass callback-handle (js-value-handle)
  ((cb :initarg :cb
       :reader cb)
   (persistent :initarg :persistent
	       :reader persistentp)))

(defun make-callback (fn &key persistent)
  (flet ((make-callback-js-code (id)
	   (concatenate 'string
			"(function(){ return process.natives.cl_call.apply(null, ['"
			id
			"'].concat(Array.prototype.slice.call(arguments))); })")))
  (let* ((id (string (gensym)))
	 (js-value (js-value (evaluate (make-callback-js-code id)))))
    (setf (gethash id *callbacks*)
	  (make-instance 'callback-handle
			 :js-value js-value
			 :persistent persistent
			 :cb fn)))))

(define-js-callback cl-call (callback-id &rest args)
  (let* ((callback-handle (gethash callback-id *callbacks*))
	 (result (apply (cb callback-handle) args)))
    (unless (persistentp callback-handle)
      (remhash callback-id *callbacks*)
      (free callback-handle))
    (js-return result)))
  
;;; **************************************************

;;; **************************************************

      
(defun call-function (js-value-handle &rest args)
  (without-fp-traps
    (let ((num-args (length args))
	  (js-value (js-value js-value-handle))
	  (result (cffi:foreign-alloc '(:struct jx:jx-value))))
      ;; Set params
      (cffi:with-foreign-object (params '(:struct jx:jx-value) num-args)
	(loop
	   for arg in args
	   for i from 0
	   do (setf (cffi:mem-aref params '(:struct jx:jx-value) i) arg))
	;; Call function
	(jx:call-function js-value params num-args result)
	;; Free params
	(dotimes (i num-args)
	  (jx:free (cffi:mem-aptr params '(:struct jx:jx-value) i)))
	;; Return converted result, free JS result ptr if not a function
	(prog1 (convert-js-value result)
	  (when (not (jx:is-function result))
	    (jx:free result)
	    (cffi:foreign-free result)))))))

(defun evaluate (js-code)
  (without-fp-traps
    (let ((result (cffi:foreign-alloc '(:struct jx:jx-value))))
      (jx:evaluate js-code "" result)
      (prog1 (convert-js-value result)
	(when (not (jx:is-function result))
	  (jx:free result)
	  (cffi:foreign-free result))))))

;;; **************************************************

;;; **************************************************

(define-js-callback cl-evaluate (code)
  (js-return (eval (read-from-string code))))
	  
;;; **************************************************

;;; **************************************************
    
(defun init-engine ()
  (jx:initialize-once "")
  (jx:initialize-new-engine)
  (jx:define-main-file "console.log('Engine Started');")
  (jx:define-extension "cl_call" (cffi:callback cl-call))
  (jx:define-extension "cl_eval" (cffi:callback cl-evaluate)))

(defun start-engine ()
  (without-fp-traps (jx:start-engine)))

(defun stop-engine ()
  (without-fp-traps (jx:stop-engine)))

(defun tick ()
  (without-fp-traps (jx:loop-once)))

;;; **************************************************

;;; **************************************************


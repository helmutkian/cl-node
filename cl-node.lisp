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


;;; **************************************************

;;; **************************************************

(defvar *js-callbacks*
  (make-hash-table :test 'equal))

(defun register-callback (callback)
  (let ((callback-id (string (gensym))))
    (setf (gethash callback-id *js-callbacks*) callback)
    callback-id))

(defun invoke-callback (callback-id)
  (let ((callback (gethash callback-id *js-callbacks*)))
    (remhash callback-id *js-callbacks*)
    callback))

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
	  

(defun wrap-js-code (js-code)
  (concatenate 'string
	       "(function () {"
	       "var callbackId = arguments[0];"
	       "var callback = function (result) { process.natives.cl_return(callbackId, result); };"
	       "var args = Array.prototype.slice.call(arguments);"
	       "args[0] = callback;"
	       "("
	       js-code
	       ").apply(null, args);"
	       "})"))


(defun make-thunk (js-code)
  (let ((wrapped-code (wrap-js-code js-code)))
    (evaluate (wrap-js-code js-code))))

(defun run (thunk callback &rest args)
  (let ((callback-id (register-callback callback)))
    (apply #'call-function thunk callback-id args)))
  
(cffi:defcallback cl-return :void ((args :pointer) (argc :int))
  (declare (ignore argc))
  (let ((callback-id
	 (convert-js-value (cffi:mem-aptr args '(:struct jx:jx-value) 0)))
	(result
	 (convert-js-value (cffi:mem-aptr args '(:struct jx:jx-value) 1))))
    (funcall (invoke-callback callback-id) result)))

;;; **************************************************

;;; **************************************************

;; Do nothing
(cffi:defcallback void-cb :void ((args :pointer) (argc :int))
  (declare (ignore args argc)))

(defun init-engine ()
  (jx:initialize "" (cffi:callback void-cb))
  (jx:initialize-new-engine)
  (jx:define-main-file "console.log('Engine Started');")
  (jx:define-extension "cl_return" (cffi:callback cl-return)))

(defun start-engine ()
  (without-fp-traps (jx:start-engine)))

(defun stop-engine ()
  (without-fp-traps (jx:stop-engine)))

(defun tick ()
  (without-fp-traps (jx:loop-once)))

;;; **************************************************

;;; **************************************************


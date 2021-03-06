(defpackage #:cl-node.jx
  (:nicknames #:jx)
  (:use #:cl)
  (:export #:jx-type
	   #:jx-value
	   #:jx-value-type
	   #:initialize
	   #:initialize-once
	   #:initialize-new-engine
	   #:define-extension
	   #:define-main-file
	   #:start-engine
	   #:stop-engine
	   #:loop-once
	   #:evaluate
	   #:free
	   #:is-v8
	   #:is-null
	   #:is-undefined
	   #:is-null-or-undefined
	   #:is-boolean
	   #:is-int32
	   #:is-double
	   #:is-object
	   #:is-buffer
	   #:is-string
	   #:is-error
	   #:is-function
	   #:get-boolean
	   #:get-int32
	   #:get-double
	   #:get-string
	   #:set-boolean
	   #:call-function
	   #:make-persistent
	   #:clear-persistent
	   #:new
	   #:set-boolean
	   #:set-int32
	   #:set-double
	   #:set-string
	   #:set-null
	   #:set-undefined
	   #:set-native-method
	   #:create-empty-object
	   #:set-named-property
	   #:get-global-object))

(defpackage #:cl-node
  (:nicknames #:js)
  (:use #:cl)
  (:export #:js-value-handle
	   #:callback-handle
	   #:free
	   #:convert-js-type
	   #:make-js-value
	   #:make-callback
	   #:evaluate
	   #:call-function
	   #:init-engine
	   #:start-engine
	   #:stop-engine
	   #:tick))

(defpackage #:cl-node.test
  (:use #:cl)
  (:export #:run-tests))

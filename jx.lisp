(in-package :cl-node.jx)

(pushnew #p"jxcore/out/Release/"
	 cffi:*foreign-library-directories*)

(cffi:define-foreign-library libjx
  (:unix (:or "libjx.so")))

(cffi:defctype size :unsigned-long)

(cffi:defctype bool :char)

(cffi:defcenum jx-type
  (:rt-int32 1)
  :rt-double
  :rt-boolean
  :rt-string
  :rt-object
  :rt-buffer
  :rt-undefined
  :rt-null
  :rt-error
  :rt-function)

(cffi:defcstruct (jx-value :class jx-value-type)
  (com :pointer)
  (persistant bool)
  (was-stored bool)
  (data :pointer)
  (size size) 
  (type :int))
    
(cffi:use-foreign-library libjx)	

;; JX_Initialize depricated in favor of JX_InitializeOnce
(cffi:defcfun ("JX_Initialize" initialize) :void (home-folder :string) (callback :pointer))

(cffi:defcfun ("JX_InitializeOnce" initialize-once) :void (home-folder :string))

(cffi:defcfun ("JX_InitializeNewEngine" initialize-new-engine)  :void)

(cffi:defcfun ("JX_DefineMainFile" define-main-file) :void (data :string))

(cffi:defcfun ("JX_DefineExtension" define-extension) :void (name :string) (callback :pointer))

(cffi:defcfun ("JX_StartEngine" start-engine) :void)

(cffi:defcfun ("JX_LoopOnce" loop-once) :int)

(cffi:defcfun ("JX_Evaluate" evaluate) :boolean (script-code :string) (script-name :string) (result :pointer))

(cffi:defcfun ("JX_Free" free) :void (value :pointer))

(cffi:defcfun ("JX_StopEngine" stop-engine) :void)

(cffi:defcfun ("JX_IsV8" is-v8) :boolean)

(cffi:defcfun ("JX_IsNull" is-null) :boolean (result :pointer))

(cffi:defcfun ("JX_IsUndefined" is-undefined) :boolean (result :pointer))

(cffi:defcfun ("JX_IsNullOrUndefined" is-null-or-undefined):boolean (result :pointer))

(cffi:defcfun ("JX_IsBoolean" is-boolean) :boolean (result :pointer))

(cffi:defcfun ("JX_IsInt32" is-int32) :boolean (result :pointer))

(cffi:defcfun ("JX_IsDouble" is-double) :boolean (result :pointer))

(cffi:defcfun ("JX_IsBuffer" is-buffer) :boolean (result :pointer))

(cffi:defcfun ("JX_IsObject" is-object) :boolean (result :pointer))

(cffi:defcfun ("JX_IsString" is-string) :boolean (result :pointer))

(cffi:defcfun ("JX_IsError" is-error) :boolean (result :pointer))

(cffi:defcfun ("JX_IsFunction" is-function) :boolean (result :pointer))

(cffi:defcfun ("JX_GetBoolean" get-boolean) :boolean (result :pointer))

(cffi:defcfun ("JX_GetInt32" get-int32) :int32 (result :pointer))

(cffi:defcfun ("JX_GetDouble" get-double) :double (result :pointer))

(cffi:defcfun ("JX_GetString" get-string) :string (result :pointer))

(cffi:defcfun ("JX_CallFunction" call-function) :void (fnc :pointer) (params :pointer) (argc :int) (out :pointer))

(cffi:defcfun ("JX_MakePersistent" make-persistent) :boolean (value :pointer))

(cffi:defcfun ("JX_ClearPersistent" clear-persistent) :boolean (value :pointer))

(cffi:defcfun ("JX_New" new) :boolean (value :pointer))

(cffi:defcfun ("JX_SetBoolean" set-boolean) :void (value :pointer) (val :boolean))

(cffi:defcfun ("JX_SetInt32" set-int32) :void (value :pointer) (val :int32))

(cffi:defcfun ("JX_SetDouble" set-double) :void (value :pointer) (val :double))

(cffi:defcfun ("JX_SetString" set-string) :void (value :pointer) (val :string) (len :int))

(cffi:defcfun ("JX_SetNull" set-null) :void (value :pointer))

(cffi:defcfun ("JX_SetUndefined" set-undefined) :void (value :pointer))




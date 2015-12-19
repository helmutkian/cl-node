# cl-node

CL-Node allows you to run Node.js as a thread within your Common Lisp process. You can evaluate and invoke JavaScript code on the fly.

CL-Node is built on JXCore: https://github.com/jxcore/jxcore . You will need to clone the JXCore repo and compile it as a shared library following the directions here: https://github.com/jxcore/jxcore/blob/master/doc/HOW_TO_COMPILE.md#compile-as-a-dynamic-library .

This project is **pre-alpha** and is therefore highly unstable. More documentation is to come.

## Working Examples

````common-lisp

(js:init-engine)
(js:start-engine)

;; Return a closure from JS and call it
(defvar js-fn (js:evaluate "var x = 0; (function () { return x++; });"))

(js:call-function js-fn)
; => 0
(js:call-function js-fn)
; => 1

(js:free js-fun)

;; Return a closure from JS that can asynchronously return control flow to Common Lisp

(defvar js-thunk (js:make-thunk "function (cl_return, x, y) { setTimeout(function () { cl_return(x+y); }, 0); }"))

(js:run js-thunk (lambda (result) (print result)) 11 22)

; Tick forward event loop to execute timeout function
(js:tick)

; printed: 33

(js:free js-thunk)

(js:stop-engine)
````


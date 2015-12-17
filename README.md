# cl-node

CL-Node allows you to run Node.js as a thread within your Common Lisp process. You can evaluate and invoke JavaScript code on the fly.

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
````


# cl-node

CL-Node allows you to run Node.js as a thread within your Common Lisp process. You can evaluate and invoke JavaScript code on the fly.

CL-Node is built on JXCore: https://github.com/jxcore/jxcore . 

This project is **pre-alpha** and is therefore highly unstable. More documentation is to come.

## Installation

1. Clone this repository.
2. Download the source for JXCore 3.1.0: https://github.com/jxcore/jxcore/releases/tag/v0.3.1.0
3. Unzip the JXCore source into a subdirectory of CL-Node
4. Within the JXCore directory, configure JXCore to build as a shared library ``./configure --shared-library``. This will build JXCore with V8 as the JavaScript engine. If you wish to use a different engine (SpiderMonkey, Chakra, etc), please see the JXCore documentation: https://github.com/jxcore/jxcore/blob/master/doc/HOW_TO_COMPILE.md
5. Within the JXCore directory, build JXCore ``make``
6. Fire up your Common Lisp environment
7. Load the cl-node.asd file ``(load "<path to cl-node directory>/cl-node.asd")``
8. Use Quicklisp to load the system and all it's dependencies ``(ql:quickload 'cl-node)``

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


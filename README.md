# cl-node

CL-Node allows you to run Node.js as a thread within your Common Lisp process. You can evaluate and invoke JavaScript code on the fly.

CL-Node is built on JXCore: https://github.com/jxcore/jxcore . 

This project is **pre-alpha** and is therefore highly unstable. More documentation is to come.

## Installation

1. Clone this repository.
2. Download the source for JXCore 0.3.1.1: https://github.com/jxcore/jxcore/tree/0.3.1.1
3. Clone or unzip JXCore into your CL-Node directory
4. Within the JXCore directory, configure & build JXCore as a shared library, following these instructions for your platform: https://github.com/jxcore/jxcore/blob/master/doc/HOW_TO_COMPILE.md#compile-as-a-dynamic-library
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

(js:free js-fn)

;; Return a closure from JS that can asynchronously return control flow to Common Lisp

(defvar js-fn2 (js:evaluate "(function (cb) { setTimeout(function () { cb('foo'); }, 0); })"))

(js:call-function js-fn2 (js:make-callback (lambda (x) (print x))))

; Tick forward event loop to execute timeout function
(js:tick)

; printed: foo

(js:free js-fn2)

;; Evaluate Common Lisp expressions from inside JS

(js:evaluate "('foo' + CL.evaluate('(concatenate \\'string \"ba\" \"r\")'))")
=> "foobar"

(js:stop-engine)
````

## Road Map

These are the major TODO items before can CL-Node enter the alpha stage

* Tests
* Documentation
* ~~Being able to pass callbacks from Common Lisp into JavaScript that can be invoked repeatedly.~~ 
    * Currently re-invokable callbacks must live as long as the JavaScript engine is still running since it's unknown when it will be garbage collected within the JavaScript engine. This could yield memory leaks if used carelessly. This issue will be resolved when JXCore allows for weak references (see this JXCore issue: https://github.com/jxcore/jxcore/issues/742).
* Handling binary data being passed into and out of JavaScript. What to do with JS Buffer objects?
* Since the JavaScript engine runs on a separate thread, a mechanism for joining back to the parent CL thread. Perhaps using a blocking ``with-event-loop`` construct such as cl-async (https://github.com/orthecreedence/cl-async) has.
* Allowing for definitions of JavaScript classes to prevent serialization/deserialization of JS objects when being returned from JavaScript functions. (Currently JS Object and Array object are returned as JSON).
* Portable automated build for downloading and compiling the JXCore shared library.
* Handling JavaScript errors

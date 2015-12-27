(in-package :cl-node.test)

(5am:def-suite cl-node.test)

(5am:in-suite cl-node.test)

(5am:test test-closure-from-js
  (let ((js-fn (js:evaluate "var x = 0; (function () { return x++; });")))
    (5am:is (= 0 (js:call-function js-fn)))
    (5am:is (= 1 (js:call-function js-fn)))
    (js:free js-fn)))

(5am:test test-async-return-to-cl
  (let ((js-fn (js:evaluate "(function (cb) { cb('test'); });"))
	(result nil))
    (js:call-function js-fn
		      (js:make-callback (lambda (x) (setf result x))))
    (js:tick)
    (5am:is (string= "test" result))))

(defun run-tests ()
  (5am:run! 'cl-node.test))

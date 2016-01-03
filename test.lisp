(in-package :cl-node.test)

(5am:def-suite cl-node.test)

(5am:in-suite cl-node.test)

(5am:test test-closure-from-js
  (let ((js-fn (js:evaluate "var x = 0; (function () { return x++; });")))
    (5am:is (= 0 (js:call-function js-fn)))
    (5am:is (= 1 (js:call-function js-fn)))
    (js:free js-fn)))

(5am:test test-cl-callback
  (let ((js-fn (js:evaluate "(function (cb) { cb('test'); });"))
	(result nil))
    (js:call-function js-fn
		      (js:make-callback
		       (lambda (x)
			 (setf result x)
			 ;; Return null
			 (values))))
    (5am:is (string= "test" result))
    (js:free js-fn)))


(5am:test test-cl-callback-async
  (let ((js-fn (js:evaluate "(function (cb) { setTimeout(function () { cb('foo'); }, 0); })"))
	(result nil))
    (js:call-function js-fn
		      (js:make-callback
		       (lambda (x)
			 (setf result x)
			 ;; Return null
			 (values))))
    (5am:is (null result))
    (loop until (zerop (js:tick)))
    (5am:is (string= "foo" result))))


(5am:test test-return-from-cl-callback-to-js
  (let ((js-fn (js:evaluate "(function (f) { return f(); })")))
    (5am:is (= 111
	       (js:call-function js-fn (js:make-callback (lambda () 111)))))))
  
(5am:test test-cl-evaluate
  (5am:is
   (= 7 (js:evaluate "(1 + process.natives.cl_eval('(+ 1 2 3)'))"))))

(defun run-tests ()
  (5am:run! 'cl-node.test))

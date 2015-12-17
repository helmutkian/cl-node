(asdf:defsystem #:cl-node
  :description "cl-node"
  :description "Embedded NodeJs runtime for Common Lisp"
  :author "Helmut Kian Rohrbacher <github.com/helmutkian>"
  :license "MIT"
  :depends-on (#:cffi)
  :components ((:file "package")
	       (:file "jx"
		      :depends-on ("package"))
	       (:file "cl-node"
		      :depends-on ("package" "jx"))))


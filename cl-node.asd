(asdf:defsystem #:cl-node
  :description "cl-node"
  :description "Embedded NodeJs runtime for Common Lisp"
  :author "Helmut Kian Rohrbacher <github.com/helmutkian>"
  :license "MIT"
  :depends-on (#:cffi #:fiveam)
  :components ((:file "package")
	       (:file "jx"
		      :depends-on ("package"))
	       (:file "cl-node"
		      :depends-on ("package" "jx"))
	       (:file "test"
		      :depends-on ("package" "cl-node"))))


;; TODO: Break tests out as their own ASDF system

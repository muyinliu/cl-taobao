;;;; cl-taobao-test.asd

(asdf:defsystem :cl-taobao-test
  :serial t
  :version "0.1"
  :description "Taobao Open API(http://open.taobao.com/) Common Lisp SDK tester."
  :depends-on (:cl-taobao)
  :components ((:file "cl-taobao-test")))
;;;; cl-taobao.asd

(asdf:defsystem :cl-taobao
  :serial t
  :version "0.1"
  :description "Taobao Open API(http://open.taobao.com/) Common Lisp SDK."
  :depends-on (:drakma
               :jsown
               :cl-ppcre
               :babel
               :ironclad)
  :components ((:static-file "cl-taobao.asd")
               (:file "packages")
               (:file "cl-taobao")))


;;;; packages.lisp

(defpackage :cl-taobao
  (:use :cl)
  (:nicknames :taobao :tb)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export :make-top-client
           :execute))

(pushnew :cl-taobao *features*)

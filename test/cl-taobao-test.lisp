;;;; test.lisp

(defpackage :taobao-test
  (:use :cl :taobao)
  (:nicknames :tb-test)
  #+:sbcl (:shadow :defconstant)
  #+:sb-package-locks (:lock t)
  (:export :test))

(in-package :taobao-test)

;; TODO
(defvar *taobaoke-app-key* "put your app key here")
(defvar *taobaoke-app-secret* "put your app secret here")

(defvar *taobaoke-api-url*         "https://eco.taobao.com/router/rest")
(defvar *taobaoke-api-sandbox-url* "https://gw.api.tbsandbox.com/router/rest")

(defvar *taobaoke-api-item-get*           "taobao.tbk.item.get"
  "淘宝客商品查询")
(defvar *taobaoke-api-item-recommend-get* "taobao.tbk.item.recommend.get"
  "淘宝客商品关联推荐查询")
(defvar *taobaoke-api-item-info-get*      "taobao.tbk.item.info.get"
  "淘宝客商品详情（简版）")
(defvar *taobaoke-api-shop-get*           "taobao.tbk.shop.get"
  "淘宝客店铺查询")
(defvar *taobaoke-api-shop-recommend-get* "taobao.tbk.shop.recommend.get"
  "淘宝客店铺关联推荐查询")

(defun test ()
  (let ((top-client (taobao:make-top-client *taobaoke-app-key*
                                            *taobaoke-app-secret*
                                            *taobaoke-api-url*)))
    (taobao:execute top-client
                    ;; api-name
                    *taobaoke-api-item-info-get*
                    ;; parameter-alist (key's value should be string or jsown object)
                    '(("fields" . "num_iid,title,pict_url,small_images,reserve_price,zk_final_price,user_type,provcity,item_url,nick,seller_id,volume")
                      ;; product id
                      ("num_iids" . "43984985414"))
                    ;; callback function
                    #'(lambda (condition data)
                        (values condition data)))))

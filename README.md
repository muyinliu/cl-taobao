#cl-taobao

cl-taobao is a Common Lisp SDK of [Taobao Open API](http://open.taobao.com/), also known as top-client.

##License
Copyright (c) 2016 Muyinliu Xing
Released under the ISC License.

##Dependencies
Relax, usually Quicklisp will download all these packages for you :)
* [drakma](https://github.com/edicl/drakma)
* [jsown](https://github.com/madnificent/jsown)
* [cl-ppcre](http://www.weitz.de/cl-ppcre/)
* [babel](https://travis-ci.org/cl-babel/babel)
* [ironclad](https://github.com/froydnj/ironclad)

##Install and load with QuickLisp
In shell:
```shell
git clone https://github.com/muyinliu/cl-taobao.git
cp -r cl-taobao ~/quicklisp/local-projects/
```
Then in Common Lisp:
```lisp
(ql:quickload 'cl-taobao)
```

##Usage
###Create top-client
```lisp
(defvar *top-client* 
        (taobao:make-top-client "taobao-app-key"
                                "taobao-app-secret"
                                "taobao-api-url"))
```
Will get:
```=>
*TOP-CLIENT*
```
Note: Please replace your own **taobao-app-key**, **taobao-app-secret** and valid **taobao-api-url** here

###Make request
```lisp
(taobao:execute *top-client*
                "taobao.tbk.item.info.get"
                '(("fields" . "num_iid,title,pict_url,small_images,reserve_price,zk_final_price,user_type,provcity,item_url,nick,seller_id,volume")
                  ("num_iids" . "43984985414"))
                #'(lambda (condition data)
                    (values condition data))
```
Will get something like this:
```=>
NIL
(:OBJ
 ("results" :OBJ
  ("n_tbk_item"
   (:OBJ ("item_url" . "http://item.taobao.com/item.htm?id=43984985414")
    ("nick" . "burton旗舰店") ("num_iid" . 43984985414)
    ("pict_url"
     . "http://img3.tbcdn.cn/tfscom/i1/TB1.AIMJpXXXXamXVXXXXXXXXXX_!!0-item_pic.jpg")
    ("provcity" . "北京") ("reserve_price" . "199.00") ("seller_id" . 2120031271)
    ("small_images" :OBJ
     ("string"
      "http://img4.tbcdn.cn/tfscom/i4/TB1aWOhHpXXXXaWXpXXXXXXXXXX_!!0-item_pic.jpg"
      "http://img3.tbcdn.cn/tfscom/i3/2120031271/TB2e3V6cVXXXXXyXpXXXXXXXXXX_!!2120031271.jpg"
      "http://img3.tbcdn.cn/tfscom/i4/TB1b0UdIFXXXXaqXVXXXXXXXXXX_!!0-item_pic.jpg"
      "http://img1.tbcdn.cn/tfscom/i4/2120031271/TB2r7F2cVXXXXaFXpXXXXXXXXXX_!!2120031271.jpg"))
    ("title" . "BURTON美国潮牌 Beeracuda户外旅行单肩背隔热啤酒包") ("user_type" . 1)
    ("volume" . 2) ("zk_final_price" . "199.00"))))
 ("request_id" . "z296a8kkqib5"))
```
Note: `condition` could be `NIL`, `jsown-parse-error`, `top-client-error` OR other conditions.

Note: `data` is `NIL` or [JSOWN](https://github.com/madnificent/jsown) object

##More
Welcome to reply.
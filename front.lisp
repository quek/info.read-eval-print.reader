(in-package :info.read-eval-print.reader)


(defparameter *js-path* (merge-pathnames "js/" *default-directory*)
  "JavaScript 用ディレクトリ")
(defparameter *css-path* (merge-pathnames "css/" *default-directory*)
  "スタイルシート用ディレクトリ")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Web server
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setf
 ;; for utf-8
 hunchentoot:*hunchentoot-default-external-format* (flexi-streams:make-external-format :utf-8)
 hunchentoot:*default-content-type* "text/html; charset=utf-8"
 ;; for debug
 hunchentoot:*catch-errors-p* nil)

(setf hunchentoot:*dispatch-table*
      (list
       'hunchentoot:dispatch-easy-handlers
       (hunchentoot:create-folder-dispatcher-and-handler "/css/" *css-path*)
       (hunchentoot:create-folder-dispatcher-and-handler "/js/" *js-path*)))

(defvar *acceptor*)

(defun start (&optional (port 8888))
  "Web サーバ起動"
  (setf *acceptor* (hunchentoot:start
                    (make-instance 'hunchentoot:acceptor
                                   :port port))))

(defun stop ()
  "Web サーバ停止"
  (hunchentoot:stop *acceptor*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ビュー
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *login-user* nil "ログインユーザ")

(defvar *messages* nil "入力エラーメッセージ")

(defun messages ()
  *messages*)

(defun add-message (message)
  (setf *messages* (nconc *messages* (list message))))

(defun clear-message ()
  (setf *messages* nil))

(defun login (user redirect-url)
  "ログイン処理"
  (setf *login-user* user)
  (when hunchentoot:*session*
    (hunchentoot:remove-session hunchentoot:*session*))
  (hunchentoot:start-session)
  (setf (hunchentoot:session-value 'login-user-id) (rucksack:object-id user))
  (hunchentoot:redirect redirect-url))

(defun logout (redirect-url)
  "ログアウト処理"
  (when hunchentoot:*session*
    (hunchentoot:remove-session hunchentoot:*session*))
  (setf *login-user* nil)
  (hunchentoot:redirect redirect-url))

(setf *prologue* "<!DOCTYPE html>")
(setf *attribute-quote-char* #\")

(defmacro with-default-template ((&key (title "題名")
                                       (charset "UTF-8")) &body body)
  "ページのテンプレート"
  `(with-html-output-to-string (out nil :prologue t :indent t)
     (htm (:html :lang "ja"
                 (:head
                  (:meta :charset ,charset)
                  (:title ,title)
                  (:link :rel "stylesheet" :href "/css/main.css" :media "all")
                  (:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js")
                  (:script :src "/js/common.js"))
                 (:body
                  (when (messages)
                    (htm (:ul (loop for message in (messages)
                                    do (htm (:li (esc message)))))))
                  (:div
                   ,@body))))))

(defun select-login-user ()
  (let ((login-user-id (hunchentoot:session-value 'login-user-id)))
    (when login-user-id
      (rucksack:cache-get-object login-user-id (rucksack:rucksack-cache rucksack:*rucksack*)))))

(defmacro with-login-user (&body body)
  `(let ((*login-user* (select-login-user)))
     ,@body))

(defmacro define-page (description lambda-list &body body)
  "ページ定義。

description
hunchentoot:define-easy-handler に加えて
ログインが必要な場合は :login-require-p t を指定する。

lambda-list
hunchentoot:define-easy-handler と同じ。"
  (let ((login-required-p (and (listp description)
                               (getf (cdr description) :login-require-p))))
    (when (listp description)
      (remf (cdr description) :login-require-p))
    (alexandria:with-gensyms (response-body error)
      `(hunchentoot:define-easy-handler ,description ,lambda-list
         (let (,response-body ,error
               (*messages* *messages*))
           (if rucksack:*transaction*
               #1=(with-login-user
                    ,@(when login-required-p
                        `((unless *login-user*
                            ;; ログインしていな場合の処理
                            (hunchentoot:redirect "/"))))
                    (setf (values ,response-body ,error) (progn ,@body)))
               (rucksack:with-transaction ()
                 #1#))
           (values ,response-body ,error))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 各ページ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; トップページ
(define-page (%root :uri "/") ()
  (with-default-template (:title "トップ")
    (htm
     (:div :class "ba" "ブランクプロジェクト")
     (if *login-user*
         (htm (:div (str (email-of *login-user*))
                    " でログインしています。")
              (:div (:a :href "logout" "ログアウト")))
         (htm (:div (:a :href "login" "ログイン"))))
     (htm (:div (:a :href "/reader" "リーダ"))))))

;;;; ログインページ
(define-page (%login :uri "/login") (email)
  (with-default-template (:title "ログイン")
    (htm
     (:form :action "authenticate" :method :post
            (:div "email"
                  (:input :type :text :name "email" :value email))
            (:div "パスワード"
                  (:input :type :password :name "password"))
            (:div (:input :type :submit :value "ログイン"))))))

;;;; 認証
(define-page (%authenticate :uri "/authenticate")
    ((email :init-form "") (password :init-form ""))
  (when (string= "" email)
    (add-message "email を入力してください。"))
  (when (string= "" password)
    (add-message "パスワードを入力してください。"))
  (if (messages)
      (%login :email email)
      (let ((user (authenticate email password)))
        (if user
            (login user "/")
            (%login :email email)))))

;;;; ログアウト
(define-page (%logout :uri "/logout") ()
  (logout "/"))

;;;; リーダ
(define-page (%reader :uri "/reader" :login-require-p t) ()
  (with-default-template (:title "リーダ")
    (htm
     (:div :id "feeds"
           "購読リスト"
           (:div (:a :href "/feeds/new" "登録"))
           (:ul
            (iterate (((category feeds) (scan-hash (subscriptions-of *login-user*))))
              (htm (:li
                    (:div :class "category" (esc category)
                          (:ul
                           (iterate ((feed (scan feeds)))
                             (htm (:li :class "feed"
                                       (:a :onclick (format nil "selectFeed('~a')"
                                                            (cl-who:escape-string (url-of feed)))
                                           (esc (title-of feed)))))))))))))
     (:div :id "entries")
     (:script :src "/reader.js"))))

(define-page (%reader.js :uri "/reader.js" :login-require-p t) ()
  (ps:ps
    (defun select-feed (url)
      (chain $
             (get
              (create url (+ "/feeds/" (encode-u-r-i-component url))
                      callback (lambda (data)
                                 (alert data))))))))

(define-page (%feed-url :uri "/uuuuuuuuuuuuuuuuu"))


(define-page (%new-feed :uri "/feeds/new" :login-require-p t) (url)
  (with-default-template (:title "フィードの登録")
    (htm (:form :action "/feeds/create" :method :post
                (:div "url" (:input :type :text :name "url" :value url :class "input-url"))
                (:div (:input :type :submit :value "登録"))))))

(define-page (%create-feed :uri "/feeds/create" :login-require-p t :default-request-type :post)
    ((url :init-form ""))
  (if (string= "" url)
      (%new-feed :url url)
      (progn
        (subscribe *login-user* (intern-feed url))
        (hunchentoot:redirect "/reader"))))

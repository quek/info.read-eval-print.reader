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
                  (:link :rel "stylesheet" :href"css/main.css" :media "all")
                  (:script :src "http://ajax.googleapis.com/ajax/libs/jquery/1/jquery.min.js"))
                 (:body ,@body)))))

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
         (let (,response-body ,error)
           (rucksack:with-transaction ()
             (with-login-user
               ,@(when login-required-p
                   `((unless *login-user*
                       ;; ログインしていな場合の処理
                       (hunchentoot:redirect "/"))))
               (setf (values ,response-body ,error) (progn ,@body))))
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
     (htm (:div (:a :href "/secret" "ログインが必要なページへのリンク"))
          (:div (:a :href "/reader" "リーダ"))))))

;;;; ログインページ
(define-page (%login :uri "/login") (email messages)
  (with-default-template (:title "ログイン")
    (htm
     (when messages
       (htm (:ul (loop for message in messages
                       do (htm (:li (str message)))))))
     (:form :action "authenticate" :method :post
            (:div "email"
                  (:input :type :text :name "email" :value email))
            (:div "パスワード"
                  (:input :type :password :name "password"))
            (:div (:input :type :submit :value "ログイン"))))))

;;;; 認証
(define-page (%authenticate :uri "/authenticate")
    ((email :init-form "") (password :init-form ""))
  (let (messages)
    (when (string= "" email)
      (push "email を入力してください。" messages))
    (when (string= "" password)
      (push "パスワードを入力してください。" messages))
    (if messages
        (%login :email email :messages (reverse messages))
        (let ((user (authenticate email password)))
          (if user
              (login user "/")
              (%login :email email))))))

;;;; ログアウト
(define-page (%logout :uri "/logout") ()
  (logout "/"))

;;;; ログインが必要なページ
(define-page (secrect :uri "/secret" :login-require-p t) ()
  (with-default-template (:title "秘密のページ")
   (htm (:p "このページはログインが必要なページです。"))))


(define-page (reader :uri "/reader" :login-require-p t) ()
  (with-default-template (:title "リーダ")
    (htm (:div :id "feeds"
               "購読リスト"
               (:ul
                (iterate (((category feeds) (scan-hash (subscriptions-of *login-user*))))
                  (htm (:li (:div (esc category)
                                  (:ul
                                   (iterate ((feed (scan feeds)))
                                     (htm (:li (esc (title-of feed)))))))))))))))

(in-package :info.read-eval-print.reader)


(iterate ((x (scan '(:index :unique))))
  (pushnew x hu.dwim.defclass-star:*allowed-slot-definition-properties*))


(defun find-rucksack (class slot value)
  (rucksack:rucksack-map-slot rucksack:*rucksack*
                              class
                              slot
                              (lambda (x)
                                (return-from find-rucksack x))
                              :equal value)
  nil)

(rucksack:with-transaction ()
  (defclass* user ()
    ((email :unique t
            :index :string-index)
     (password :initarg :password
               :initarg :plain-password)
     (subscriptions (make-hash-table :test 'equal)))
    (:index t)
    (:metaclass rucksack:persistent-class)))

(defmethod print-object ((self user) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a" (email-of self))))


(defmethod initialize-instance :after ((user user)
                                       &key plain-password
                                       &allow-other-keys)
  "make-instance で :plain-password が指定されていた場合、
password に hash-password したものを設定する。"
  (when plain-password
    (setf (password-of user) plain-password)))

(defun hash-password (password)
  "パスワードのハッシュ関数"
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array password))))

(defmethod (setf password) (password (user user))
  "パスワードのハッシュをセットする。"
  (setf (slot-value user 'password) (hash-password password)))

(defun find-user-by-email (email)
  (find-rucksack 'user 'email email))

(defun authenticate (email password)
  (let ((password (hash-password password)))
    (rucksack:rucksack-map-slot rucksack:*rucksack*
                                'user
                                'email
                                (lambda (user)
                                  (when (string= password (password-of user))
                                    (return-from authenticate user)))
                                :equal email)
    nil))

#+テストデータ作成
(rucksack:with-transaction ()
  (make-instance 'user :email "user1@example.com"
                 :plain-password "password")
  t)

;; (rucksack:with-transaction () (authenticate "user1@example.com" "password"))

#+(or)
(rucksack:with-transaction ()
  (rucksack:cache-get-object 42 (rucksack:rucksack-cache rucksack:*rucksack*)))

#+(or)
(let (users)
  (rucksack:with-transaction ()
    (rucksack:rucksack-map-class rucksack:*rucksack* 'user (lambda (x) (push x users))))
  (rucksack:with-transaction ()
    (iterate ((user (scan users)))
      (rucksack:rucksack-delete-object rucksack:*rucksack* user))))

(rs:with-transaction ()
  (defclass* feed ()
    ((url :unique t :index :string-index)
     (title)
     (link :unique t :index :string-index)
     (description)
     (creator)
     (feed-entries ()))
    (:index t)
    (:metaclass rucksack:persistent-class)))

(defmethod print-object ((self feed) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a ~a" (title-of self) (url-of self) (link-of self))))


(rs:with-transaction ()
  (defclass* feed-entry ()
    ((title)
     (link :unique t)
     (content)
     (creator)
     (published)
     (category)
     (feed))
    (:index t)
    (:metaclass rucksack:persistent-class)))

(defmethod print-object ((self feed-entry) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~a ~a" (title-of self) (link-of self))))


(defmethod subscribe ((user user) (feed feed) &optional (category "unknown"))
  (unless (loop for category being the hash-key of (subscriptions-of user)
                using (hash-value feeds)
                thereis (member feed feeds :test #'rs:p-eql))
    (push feed
          (gethash category (subscriptions-of user) '()))))



(defun find-feed (url)
  (rs:rucksack-map-slot rs:*rucksack* 'feed 'url
                        (lambda (found)
                          (return-from find-feed found))
                        :equal url))

(defun intern-feed (url)
  (or (find-feed url)
      (aif (fetch-feed url)
           (make-instance 'feed
                          :url url
                          :title (title-of it)
                          :link (link-of it)
                          :description (description-of it)
                          :creator (creator-of it)))))

#+(or)
(progn
  (let (feeds)
    (rucksack:with-transaction ()
      (rucksack:rucksack-map-class rucksack:*rucksack* 'feed (lambda (x) (push x feeds))))
    (rucksack:with-transaction ()
      (print feeds)
      (iterate ((x (scan feeds)))
        (rucksack:rucksack-delete-object rucksack:*rucksack* x))))

  (rs:with-transaction ()
    (find-feed "http://cadr.g.hatena.ne.jp/g000001/rss2"))

  (rs:with-transaction ()
    (intern-feed "http://cadr.g.hatena.ne.jp/g000001/rss2"))

  (rs:with-transaction ()
    (rs:rucksack-map-class rs:*rucksack* 'feed #'print))



  (rs:with-transaction ()
    (find-feed "http://feeds.feedburner.com/blogspot/rztf"))

  (rs:with-transaction ()
    (intern-feed "http://feeds.feedburner.com/blogspot/rztf"))

  (rs:with-transaction ()
    (rs:rucksack-map-class rs:*rucksack* 'feed #'print))



  (rs:with-transaction ()
    (let ((user (find-user-by-email "user1@example.com")))
      (rs:rucksack-map-class rs:*rucksack* 'feed
                             (^ subscribe user _feed))))


  (rs:with-transaction ()
    (let ((user (find-user-by-email "user1@example.com")))
      ; (clrhash (subscriptions-of user))
      user))

  )

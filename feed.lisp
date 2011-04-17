(in-package :info.read-eval-print.reader)


(setf drakma:*drakma-default-external-format* :utf-8)

(iterate ((x (scan '(("application" . "xml")
                     ("text" . "xml")))))
  (pushnew x drakma:*text-content-types* :test #'equal))


(defclass feed ()
  ((title :initarg :title)
   (link :initarg :link)
   (description :initarg :description)
   (creator :initarg :creator)
   (items :initform () :initarg :items)))

(defclass feed-entry ()
  ((title :initarg :title)
   (link :initarg :link)
   (content :initarg :content)
   (creator :initarg :creator)
   (pub-date :initarg :pub-date)
   (category :initarg :category)))

(defun %xv (path context)
  (xpath:string-value (xpath:evaluate path context)))

(defun read-url (url)
  (delete #\Return (drakma:http-request url)))

(defun fetch-rss (url)
  (let ((response (read-url url)))
    (parse-rss response)))

;;(info.read-eval-print.reader::fetch-rss "http://cadr.g.hatena.ne.jp/g000001/rss2")

(defun parse-rss (text)
  (xpath:with-namespaces (("dc" "http://purl.org/dc/elements/1.1/")
                          ("content" "http://purl.org/rss/1.0/modules/content/"))
    (let* ((doc (cxml:parse text (stp:make-builder)))
           (feed (make-instance 'feed
                                       :title (%xv "rss/channel/title" doc)
                                       :link (%xv "rss/channel/link" doc)
                                       :description (%xv "rss/channel/description" doc)
                                       :creator (%xv "rss/channel/dc:creator" doc))))
      (with-slots (items) feed
        (xpath:do-node-set (node (xpath:evaluate "//item" doc))
          (push (make-instance 'feed-entry
                               :title (%xv "title" node)
                               :link (%xv "link" node)
                               :content (%xv "description" node)
                               :creator (%xv "dc:creator" node)
                               :pub-date (%xv "pubDate" node)
                               :category (%xv "category" node))
                items))
        (setf items (nreverse items)))
      feed)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; atom

(defun fetch-atom (url)
  (let ((response (read-url url)))
    (parse-atom response)))

;;(fetch-atom "http://blog.livedoor.jp/chiblits/atom.xml")
;;(fetch-atom "http://feeds.feedburner.com/blogspot/rztf")

(defun parse-atom (text)
  (let ((doc (cxml:parse text (stp:make-builder))))
    (let ((namespace (progs ()
                       (scan '("http://www.w3.org/2005/Atom" "http://purl.org/atom/ns#"))
                       (choose-if (lambda (namespace)
                                    (xpath:with-namespaces ((nil namespace))
                                      (string/= (%xv "feed/title" doc) ""))))
                       (collect-first))))
      (xpath:with-namespaces ((nil namespace))
        (let ((feed (make-instance 'feed
                                          :title (%xv "feed/title" doc)
                                          :link (%xv "feed/link[@rel=\"alternate\"]/@href" doc)
                                          :description (%xv "feed/tagline" doc)
                                          :creator (%xv "feed/author/name" doc))))
          (with-slots (items) feed
            (xpath:do-node-set (node (xpath:evaluate "//entry" doc))
              (push (make-instance 'feed-entry
                                   :title (%xv "title" node)
                                   :link (%xv "link/@href" node)
                                   :content (%xv "content" node)
                                   :creator (%xv "author/name" node)
                                   :pub-date (%xv "issued|published" node)
                                   :category (%xv "category/@term" node))
                    items))
            (setf items (nreverse items)))
          feed)))))

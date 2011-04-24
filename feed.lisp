(in-package :info.read-eval-print.reader)


(setf drakma:*drakma-default-external-format* :utf-8)

(iterate ((x (scan '(("application" . "xml")
                     ("text" . "xml")))))
  (pushnew x drakma:*text-content-types* :test #'equal))


(defclass* x-feed ()
  ((url)
   (title)
   (link)
   (description)
   (creator)
   (feed-entries ())))

(defclass* x-feed-entry ()
  ((title)
   (link)
   (content)
   (creator)
   (published)
   (category)))

(defparameter *whitespace* '(#\Space
                             #\Tab
                             #\Newline
                             #\Return
                             #\Page))

(defun %xv (path context)
  (let ((value (string-trim *whitespace*
                            (xpath:string-value (xpath:evaluate path context)))))
    (if (string= "" value)
        nil
        value)))

(defun read-url (url)
  (delete #\Return (drakma:http-request url)))

;;(defun fetch-rss (url)
;;  (let ((response (read-url url)))
;;    (parse-rss response)))


(defun parse-rss (text)
  (xpath:with-namespaces (("dc" "http://purl.org/dc/elements/1.1/")
                          ("content" "http://purl.org/rss/1.0/modules/content/"))
    (let* ((doc (cxml:parse text (stp:make-builder)))
           (feed (make-instance 'x-feed
                                :title (%xv "rss/channel/title" doc)
                                :link (or (%xv "rss/channel/link" doc)
                                          ;; にげる！
                                          (return-from parse-rss nil))
                                :description (%xv "rss/channel/description" doc)
                                :creator (%xv "rss/channel/dc:creator" doc))))
      (with-slots (feed-entries) feed
        (xpath:do-node-set (node (xpath:evaluate "//item" doc))
          (push (make-instance 'x-feed-entry
                               :title (%xv "title" node)
                               :link (%xv "link" node)
                               :content (%xv "description" node)
                               :creator (%xv "dc:creator" node)
                               :published (%xv "pubDate" node)
                               :category (%xv "category" node))
                feed-entries))
        (setf feed-entries (nreverse feed-entries)))
      feed)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; atom

;;(defun fetch-atom (url)
;;  (let ((response (read-url url)))
;;    (parse-atom response)))


(defun parse-atom (text)
  (let ((doc (cxml:parse text (stp:make-builder))))
    (let ((namespace (progs ()
                       (scan '("http://www.w3.org/2005/Atom" "http://purl.org/atom/ns#"))
                       (choose-if (lambda (namespace)
                                    (xpath:with-namespaces ((nil namespace))
                                      (%xv "feed/title" doc))))
                       (collect-first))))
      (xpath:with-namespaces ((nil namespace))
        (let ((feed (make-instance 'x-feed
                                          :title (%xv "feed/title" doc)
                                          :link (or (%xv "feed/link[@rel=\"alternate\"]/@href" doc)
                                                    ;; にげる！
                                                    (return-from parse-atom nil))
                                          :description (%xv "feed/tagline" doc)
                                          :creator (%xv "feed/author/name" doc))))
          (with-slots (feed-entries) feed
            (xpath:do-node-set (node (xpath:evaluate "//entry" doc))
              (push (make-instance 'x-feed-entry
                                   :title (%xv "title" node)
                                   :link (%xv "link/@href" node)
                                   :content (%xv "content" node)
                                   :creator (%xv "author/name" node)
                                   :published (%xv "issued|published" node)
                                   :category (%xv "category/@term" node))
                    feed-entries))
            (setf feed-entries (nreverse feed-entries)))
          feed)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fetch-feed (url)
  (let ((response (read-url url)))
    (let ((feed (or (parse-rss response)
                    (parse-atom response))))
      (when feed
        (setf (url-of feed) url))
      feed)))

;;(fetch-feed "http://cadr.g.hatena.ne.jp/g000001/rss2")
;;(fetch-feed "http://blog.livedoor.jp/chiblits/atom.xml")
;;(fetch-feed "http://feeds.feedburner.com/blogspot/rztf")

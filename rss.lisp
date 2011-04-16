(in-package :info.read-eval-print.reader)


(setf drakma:*drakma-default-external-format* :utf-8)
(pushnew '("application" . "xml") drakma:*text-content-types* :test #'equal)


(defclass rss-channel ()
  ((title :initarg :title)
   (link :initarg :link)
   (description :initarg :description)
   (creator :initarg :creator)
   (items :initform () :initarg :items)))

(defclass rss-item ()
  ((title :initarg :title)
   (link :initarg :link)
   (description :initarg :description)
   (creator :initarg :creator)
   (pub-date :initarg :pub-date)
   (category :initarg :category)))

(defun %xv (path context)
  (xpath:string-value (xpath:evaluate path context)))

(defun fetch-rss (url)
  (let ((response (drakma:http-request url)))
    (xpath:with-namespaces (("dc" "http://purl.org/dc/elements/1.1/"))
      (let* ((doc (cxml:parse response (cxml-xmls:make-xmls-builder)))
             (xpath:*navigator* (cxml-xmls:make-xpath-navigator))
             (rss-channel (make-instance 'rss-channel
                                         :title (%xv "//channel/title" doc)
                                         :link (%xv "//channel/link" doc)
                                         :description (%xv "//channel/description" doc)
                                         :creator (%xv "//channel/dc:creator" doc))))
        (with-slots (items) rss-channel
          (xpath:do-node-set (node (xpath:evaluate "//item" doc))
            (push (make-instance 'rss-item
                                 :title (%xv "title" node)
                                 :link (%xv "link" node)
                                 :description (%xv "description" node)
                                 :creator (%xv "dc:creator" node)
                                 :pub-date (%xv "pubDate" node)
                                 :category (%xv "category" node))
                  items))
          (setf items (nreverse items)))
        rss-channel))))

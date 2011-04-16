;;;; info.read-eval-print.reader.asd

(asdf:defsystem :info.read-eval-print.reader
  :serial t
  :components ((:file "package")
               (:file "rss")
               (:file "front"))
  :depends-on (:hunchentoot
               :clsql
               :cl-who
               :ironclad
               :quek
               :drakma
               :flexi-stream-jp
               :cxml
               :xpath))


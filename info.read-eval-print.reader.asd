;;;; info.read-eval-print.reader.asd

(asdf:defsystem :info.read-eval-print.reader
  :serial t
  :components ((:file "package")
               (:file "feed")
               (:file "front"))
  :depends-on (:hunchentoot
               :rucksack
               :cl-who
               :ironclad
               :quek
               :flexi-stream-jp
               :drakma
               :cxml
               :cxml-stp
               :xpath))


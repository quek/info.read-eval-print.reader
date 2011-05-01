;;;; info.read-eval-print.reader.asd

(asdf:defsystem :info.read-eval-print.reader
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "feed")
               (:file "rucksack")
               (:file "model")
               (:file "front"))
  :depends-on (:hunchentoot
               :rucksack
               :cl-who
               :parenscript
               :ironclad
               :quek
               :flexi-stream-jp
               :drakma
               :cxml
               :cxml-stp
               :xpath
               :hu.dwim.defclass-star))


(in-package :info.read-eval-print.reader)

(defparameter *default-directory*
  (pathname (directory-namestring #.(or *compile-file-truename*
                                        *load-truename*)))
  "このファイルがあるディレクトリ")

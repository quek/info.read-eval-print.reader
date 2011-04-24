;;;; package.lisp

(defpackage :info.read-eval-print.reader
  (:use :cl :cl-who :quek)
  (:shadowing-import-from :cl-who #:str)
  (:import-from :hu.dwim.defclass-star #:defclass*)
  (:export #:start
           #:stop))


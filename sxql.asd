#|
  This file is a part of sxql project.
  Copyright (c) 2013-2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql-asd
  (:use :cl :asdf))
(in-package :sxql-asd)

(defsystem sxql
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:optima
               :iterate
               :cl-syntax-annot
               :trivial-types
               :split-sequence
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "sxql" :depends-on ("statement" "clause" "operator" "compile" "composed-statement"))
                 (:file "compile" :depends-on ("sql-type"))
                 (:file "sql-type")
                 (:file "operator" :depends-on ("sql-type"))
                 (:file "clause" :depends-on ("operator"))
                 (:file "statement" :depends-on ("operator" "clause" "util"))
                 (:file "composed-statement" :depends-on ("sql-type" "operator" "clause" "statement" "util"))
                 (:file "util"))))
  :description "A SQL generator"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sxql-test))))

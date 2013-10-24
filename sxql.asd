#|
  This file is a part of sxql project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql-asd
  (:use :cl :asdf))
(in-package :sxql-asd)

(defsystem sxql
  :version "0.0.1"
  :author "Eitarow Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:optima
               :cl-syntax-annot
               :trivial-types
               :split-sequence)
  :components ((:module "src"
                :components
                ((:file "sxql" :depends-on ("statement" "clause" "operator" "compile"))
                 (:file "compile" :depends-on ("sql-type"))
                 (:file "sql-type")
                 (:file "operator" :depends-on ("sql-type"))
                 (:file "clause" :depends-on ("operator"))
                 (:file "statement" :depends-on ("operator" "clause")))))
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
  :in-order-to ((test-op (load-op sxql-test))))

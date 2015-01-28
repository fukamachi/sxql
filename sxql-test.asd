#|
  This file is a part of sxql project.
  Copyright (c) 2013-2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql-test-asd
  (:use :cl :asdf))
(in-package :sxql-test-asd)

(defsystem sxql-test
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:sxql
               :prove)
  :components ((:module "t"
                :serial t
                :components
                ((:file "prepare")
                 (:test-file "sql-type")
                 (:test-file "operator")
                 (:test-file "clause")
                 (:test-file "statement")
                 (:test-file "composed-statement")
                 (:test-file "sxql"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern (string :run-test-system) :prove) c)))

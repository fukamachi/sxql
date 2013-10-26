#|
  This file is a part of sxql project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql-test-asd
  (:use :cl :asdf))
(in-package :sxql-test-asd)

(defsystem sxql-test
  :author "Eitarow Fukamachi"
  :license "BSD 3-Clause"
  :depends-on (:sxql
               :cl-test-more)
  :components ((:module "t"
                :serial t
                :components
                ((:file "prepare")
                 (:file "sql-type")
                 (:file "operator")
                 (:file "clause")
                 (:file "statement")
                 (:file "sxql"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))

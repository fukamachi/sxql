(defsystem "sxql"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("trivia"
               "alexandria"
               "cl-package-locks")
  :pathname "src"
  :components
  ((:file "util")
   (:file "sql-type")
   (:file "operator" :depends-on ("sql-type"))
   (:file "clause" :depends-on ("operator"))
   (:file "statement" :depends-on ("operator" "clause" "util"))
   (:file "composed-statement" :depends-on ("sql-type" "operator" "clause" "statement" "util"))
   (:file "composer" :depends-on ("sql-type" "clause" "statement"))
   (:file "compile" :depends-on ("sql-type"))
   (:file "sxql" :depends-on ("composer" "statement" "clause" "operator" "compile" "composed-statement")))
  :description "A SQL generator"
  :in-order-to ((test-op (test-op "sxql-test"))))

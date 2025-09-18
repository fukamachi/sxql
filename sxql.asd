(defsystem "sxql"
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("trivia"
               "iterate"
               "cl-annot"
               "trivial-types"
               "split-sequence"
               "named-readtables"
               "alexandria"
               "cl-package-locks")
  :pathname "src"
  :components
  ((:file "syntax")
   (:file "util")
   (:file "sql-type" :depends-on ("syntax"))
   (:file "operator" :depends-on ("sql-type" "syntax"))
   (:file "clause" :depends-on ("operator" "syntax"))
   (:file "statement" :depends-on ("operator" "clause" "syntax" "util"))
   (:file "composed-statement" :depends-on ("sql-type" "operator" "clause" "statement" "syntax" "util"))
   (:file "compile" :depends-on ("sql-type" "syntax"))
   (:file "sxql" :depends-on ("statement" "clause" "operator" "compile" "composed-statement" "syntax")))
  :description "A SQL generator"
  :in-order-to ((test-op (test-op "sxql-test"))))

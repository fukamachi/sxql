(defsystem "sxql-test"
  :author "Eitaro Fukamachi"
  :license "BSD 3-Clause"
  :depends-on ("sxql"
               "rove")
  :pathname "test"
  :serial t
  :components
  ((:file "prepare")
   (:file "sql-type")
   (:file "operator")
   (:file "clause")
   (:file "statement")
   (:file "composed-statement")
   (:file "sxql")
   (:file "sxql-v2"))
  :perform (test-op (op c) (symbol-call :rove :run c)))

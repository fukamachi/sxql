(in-package :cl-user)
(defpackage t.sxql.statement
  (:use :cl
        :sxql
        :sxql.sql-type
        :sxql.operator
        :sxql.clause
        :sxql.statement
        :cl-test-more))
(in-package :t.sxql.statement)

(plan nil)

(diag "statement")

(ok (make-statement :select
                    (make-sql-keyword "*")
                    (make-clause :from (make-sql-symbol "table-name"))
                    (make-clause :where
                                 (make-op := 'name "Eitarow"))))

(is (yield (make-statement :select
                           (make-sql-keyword "*")
                           (make-clause :from (make-sql-symbol "table-name"))
                           (make-clause :where
                                               (make-op := 'name "Eitarow"))))
    "SELECT * FROM `table-name` WHERE (`name` = ?)")

(is (yield (make-statement :select
                           (make-sql-list
                            (make-sql-symbol "a")
                            (make-sql-symbol "b"))
                           (make-clause :from (make-sql-symbol "table-name"))
                           (make-clause :where
                                               (make-op := 'name "Eitarow"))))
    "SELECT (`a`, `b`) FROM `table-name` WHERE (`name` = ?)")

(is (multiple-value-list
     (yield (make-statement :select (make-op :+ 1 1))))
    (list "SELECT (? + ?)" '(1 1)))

(is (multiple-value-list
     (yield (make-statement :select
                            (make-sql-list
                             (make-sql-symbol "a")
                             (make-sql-variable 1))
                            (make-clause :from
                                         (make-sql-symbol "table"))
                            (make-clause :order-by
                                         (make-sql-symbol "a")))))
    (list "SELECT (`a`, ?) FROM `table` ORDER BY `a`" '(1)))

(finalize)

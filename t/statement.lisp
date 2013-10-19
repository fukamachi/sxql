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
                    :from (make-clause :from (make-sql-symbol "table-name"))
                    :where (make-clause :where
                                        (make-op := 'name "Eitarow"))))

(is (stringify (make-statement :select
                               :from (make-clause :from (make-sql-symbol "table-name"))
                               :where (make-clause :where
                                                   (make-op := 'name "Eitarow"))))
    "SELECT * FROM `table-name` WHERE (`name` = ?)")

(is (stringify (make-statement :select
                               :field (make-clause :field
                                                   (make-sql-atom-list
                                                    (list (make-sql-symbol "a")
                                                          (make-sql-symbol "b"))))
                               :from (make-clause :from (make-sql-symbol "table-name"))
                               :where (make-clause :where
                                                   (make-op := 'name "Eitarow"))))
    "SELECT (`a`, `b`) FROM `table-name` WHERE (`name` = ?)")

(is (multiple-value-list
     (stringify (make-statement :select
                                :field (make-clause :field (make-op :+ 1 1)))))
    (list "SELECT (? + ?)" '(1 1)))

(is (multiple-value-list
     (stringify (make-statement :select
                                :field (make-clause :field
                                                    (make-sql-atom-list
                                                     (list (make-sql-symbol "a")
                                                           (make-sql-variable 1))))
                                :from (make-clause :from
                                                   (make-sql-symbol "table"))
                                :order-by (make-clause :order-by
                                                       (make-sql-symbol "a")))))
    (list "SELECT (`a`, ?) FROM `table` ORDER BY `a`" '(1)))

(finalize)

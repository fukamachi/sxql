(in-package :cl-user)
(defpackage t.sxql.statement
  (:use :cl
        :sxql
        :sxql.sql-type
        :sxql.operator
        :sxql.clause
        :sxql.statement
        :cl-test-more)
  (:shadowing-import-from :t.sxql.prepare
                          :is-error))
(in-package :t.sxql.statement)

(plan 15)

(diag "statement")

(ok (make-statement :select
                    (make-sql-keyword "*")
                    (make-clause :from (make-sql-symbol "table-name"))
                    (make-clause :where
                                 (make-op := :name "Eitarow"))))

(is (yield (make-statement :select
                           (make-sql-keyword "*")
                           (make-clause :from (make-sql-symbol "table-name"))
                           (make-clause :where
                                        (make-op := :name "Eitarow"))))
    "SELECT * FROM `table-name` WHERE (`name` = ?)")

(is (yield (make-statement :select
                           (make-sql-list
                            (make-sql-symbol "a")
                            (make-sql-symbol "b"))
                           (make-clause :from (make-sql-symbol "table-name"))
                           (make-clause :where
                                        (make-op := :name "Eitarow"))))
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

(is (multiple-value-list
     (yield (make-statement :insert-into (make-sql-symbol "table")
                            (make-clause :set=
                                         (make-sql-symbol "a")
                                         (make-sql-variable 10)))))
    '("INSERT INTO `table` (`a`) VALUES (?)" (10)))

(is (multiple-value-list
     (yield (make-statement :update (make-sql-symbol "table")
                            (make-clause :set=
                                         (make-sql-symbol "a")
                                         (make-sql-variable 10)
                                         (make-sql-symbol "b")
                                         (make-sql-variable 20))
                            (make-clause :where
                                         (make-op :>
                                                  (make-sql-symbol "age")
                                                  (make-sql-variable 20)))
                            (make-clause :order-by
                                         (make-sql-symbol "id"))
                            (make-clause :limit 5))))
    '("UPDATE `table` SET `a` = ?, `b` = ? WHERE (`age` > ?) ORDER BY `id` LIMIT 5" (10 20 20))
    "UPDATE")

(is (multiple-value-list
     (yield (make-statement :create-table
             :enemy
             '((:name :type string
                      :primary-key t)
               (:age :type integer
                     :not-null t)
               (:address :type text
                         :not-null nil)
               (:fatal_weakness :type text
                                :not-null t
                                :default "None")
               (:identifying_color :type (:char 20)
                                  :unique t)))))
    '("CREATE TABLE `enemy` (`name` STRING PRIMARY KEY, `age` INTEGER NOT NULL, `address` TEXT, `fatal_weakness` TEXT NOT NULL DEFAULT ?, `identifying_color` CHAR(20) UNIQUE)" ("None"))
    "CREATE TABLE")

(diag "sql-compile statement")

(let ((stmt (sql-compile
             (make-statement :select
                             (make-sql-keyword "*")
                             (make-clause :from (make-sql-symbol "table-name"))
                             (make-clause :where
                                          (make-op :<
                                                   (make-sql-symbol "age")
                                                   (make-sql-variable 20)))))))
  (ok stmt)

  (is (multiple-value-list (yield stmt))
      '("SELECT * FROM `table-name` WHERE (`age` < ?)" (20)))

  (let ((union-stmt
          (make-op :union stmt (make-statement :select
                                               (make-sql-keyword "*")
                                               (make-clause :from (make-sql-symbol "table-2"))))))
    (ok union-stmt)
    (is (multiple-value-list (yield union-stmt))
        '("(SELECT * FROM `table-name` WHERE (`age` < ?) UNION SELECT * FROM `table-2`)" (20)))))

(let ((stmt (sxql:make-statement :select :* (sxql:make-clause :from :table))))
  (is (multiple-value-list (yield stmt))
      '("SELECT * FROM `table`" nil))

  (ok (add-child stmt
                 (make-clause :where
                              (make-op := :name "Eitarow"))))

  (is (multiple-value-list (yield stmt))
      '("SELECT * FROM `table` WHERE (`name` = ?)" ("Eitarow"))))

(finalize)

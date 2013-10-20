(in-package :cl-user)
(defpackage t.sxql.clause
  (:use :cl
        :cl-test-more
        :sxql.sql-type
        :sxql.operator
        :sxql.clause))
(in-package :t.sxql.clause)

(plan nil)

(ok (make-clause :where (make-op := 'a 10)))
(is (multiple-value-list
     (yield (make-clause :where (make-op := 'a 10))))
    (list "WHERE (`a` = ?)" '(10)))
(is-error (make-clause :where
                       (make-op := 'a 10)
                       (make-op :!= 'b 20))
          program-error)

(ok (make-clause :from (make-sql-symbol "table-name")))
(ok (make-clause :from (make-op :as 'table-name 'a)))
(is (multiple-value-list
     (yield (make-clause :from (make-sql-symbol "table-name"))))
    (list "FROM `table-name`" nil))
(is (multiple-value-list
     (yield (make-clause :from
                             (make-op :as 'table-name 'a))))
    (list "FROM (`table-name` AS `a`)" nil))

(ok (make-clause :field (make-sql-symbol "a")))
(is (multiple-value-list
     (yield
      (make-clause :field
                   (make-sql-symbol "a"))))
    (list "`a`" nil))
(is (multiple-value-list
     (yield
      (make-clause :field
                   (make-sql-list (make-sql-symbol "a")
                                  (make-sql-symbol "b")))))
    (list "(`a`, `b`)" nil))

(ok (make-clause :order-by (make-sql-symbol "a")))
(is (multiple-value-list
     (yield
      (make-clause :order-by (make-sql-symbol "a"))))
    (list "ORDER BY `a`" nil)
    "ORDER BY")
(is (multiple-value-list
     (yield
      (make-clause :order-by
                   (make-sql-list
                    (make-sql-symbol "a")
                    (make-sql-symbol "b")))))
    (list "ORDER BY (`a`, `b`)" nil))
(is (multiple-value-list
     (yield
      (make-clause :order-by
                   (make-sql-list
                    (make-op :desc (make-sql-symbol "a"))
                    (make-sql-symbol "b")))))
    (list "ORDER BY (`a` DESC, `b`)" nil))

(ok (make-clause :group-by (make-sql-symbol "a")))
(ok (make-clause :group-by
                 (make-sql-list
                  (make-sql-symbol "a")
                  (make-sql-symbol "b"))))
(ok (make-clause :group-by (make-op :+
                                    (make-sql-symbol "a")
                                    (make-sql-variable 1))))
(is (multiple-value-list
     (yield
      (make-clause :group-by (make-sql-symbol "a"))))
    (list "GROUP BY `a`" nil))
(is (multiple-value-list
     (yield
      (make-clause :group-by
                   (make-sql-list
                    (make-sql-symbol "a")
                    (make-sql-symbol "b")))))
    (list "GROUP BY (`a`, `b`)" nil))
(is (multiple-value-list
     (yield
      (make-clause :group-by
                   (make-op :+ (make-sql-symbol "a") (make-sql-variable 1)))))
    (list "GROUP BY (`a` + ?)" '(1)))

(ok (make-clause :limit (make-sql-variable 1)))
(ok (make-clause :limit (make-sql-variable 0) (make-sql-variable 10)))
(is (multiple-value-list
     (yield
      (make-clause :limit (make-sql-variable 1))))
    (list "LIMIT 1" nil))
(is (multiple-value-list
     (yield
      (make-clause :limit
                   (make-sql-variable 0)
                   (make-sql-variable 10))))
    (list "LIMIT 0, 10" nil))
(is-error (make-clause :limit (make-sql-symbol "a")) type-error)
(is-error (make-clause :limit
                       (make-sql-variable 1)
                       (make-sql-variable 2)
                       (make-sql-variable 2))
          program-error)

(ok (make-clause :offset (make-sql-variable 1)))
(is (multiple-value-list
     (yield
      (make-clause :offset (make-sql-variable 1000))))
    (list "OFFSET 1000" nil))
(is-error (make-clause :offset
                       (make-sql-variable 1)
                       (make-sql-variable 2))
          program-error)

(finalize)

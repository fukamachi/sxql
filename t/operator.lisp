(in-package :cl-user)
(defpackage t.sxql.operator
  (:use :cl
        :sxql
        :sxql.sql-type
        :sxql.operator
        :prove)
  (:shadowing-import-from :t.sxql.prepare
                          :is-error))
(in-package :t.sxql.operator)

(plan nil)

(ok (make-unary-op "NOT" (make-sql-variable 1)))
(ok (make-unary-op "NOT" (make-unary-op "NOT" (make-sql-variable 1))))
(is-error (make-unary-op "NOT" 1) type-error)

(ok (make-infix-op "=" (make-sql-variable 1) (make-sql-variable 1)))
(ok (make-infix-op "="
                   (make-unary-op "NOT" (make-sql-variable 1))
                   (make-sql-variable 1)))
(is-error (make-infix-op "=" 1 (make-sql-variable 1)) type-error)

(is-error (make-conjunctive-op "+" 1) type-error)
(ok (make-conjunctive-op "+" (make-sql-variable 1)))
(ok (make-conjunctive-op "+"
                         (make-sql-variable 1)
                         (make-sql-variable 3)
                         (make-sql-list (make-sql-symbol "a")
                                        (make-sql-symbol "b"))
                         (make-sql-keyword "NULL")))

(diag "unary-op")

(is (multiple-value-list
     (yield (make-op :not (make-sql-variable 1))))
    (list "(NOT ?)" '(1)))
(is (multiple-value-list
     (yield (make-op :is-null (make-sql-symbol "a"))))
    (list "(`a` IS NULL)" nil))
(is (multiple-value-list
     (yield (make-op :not-null (make-sql-symbol "a"))))
    (list "(`a` IS NOT NULL)" nil))

(ok (make-op :raw (make-sql-variable "SELECT * FROM table")))
(ok (make-op :raw "SELECT * FROM table"))
(is (multiple-value-list
     (yield (make-op :raw "SELECT * FROM table")))
    (list "(SELECT * FROM table)" nil))

(is-error (make-op :not
                   (make-sql-symbol "a")
                   (make-sql-variable 1))
          program-error)

(is (multiple-value-list
     (yield (make-op :distinct :age)))
    (list "DISTINCT `age`" nil))

(diag "infix-op")

(is (multiple-value-list
     (yield (make-op :=
                         (make-sql-variable 1)
                         (make-sql-variable 1))))
    (list "(? = ?)" '(1 1))
    "=")

(is (multiple-value-list
     (yield (make-op :!=
                         (make-sql-variable 1)
                         (make-sql-variable 1))))
    (list "(? != ?)" '(1 1))
    "!=")

(is (multiple-value-list
     (yield (make-op :<
                         (make-sql-variable 1)
                         (make-sql-variable 1))))
    (list "(? < ?)" '(1 1))
    "<")

(is (multiple-value-list
     (yield (make-op :>
                         (make-sql-variable 1)
                         (make-sql-variable 1))))
    (list "(? > ?)" '(1 1))
    ">")

(is (multiple-value-list
     (yield (make-op :>=
                         (make-sql-variable 1)
                         (make-sql-variable 1))))
    (list "(? >= ?)" '(1 1))
    ">=")

(is (multiple-value-list
     (yield (make-op :<=
                         (make-sql-variable 1)
                         (make-sql-variable 1))))
    (list "(? <= ?)" '(1 1))
    "<=")

(is (multiple-value-list
     (yield (make-op :a<
                     (make-sql-variable 1)
                     (make-sql-variable 1))))
    (list "(? @< ?)" '(1 1))
    "@<")

(is (multiple-value-list
     (yield (make-op :a>
                     (make-sql-variable 1)
                     (make-sql-variable 1))))
    (list "(? @> ?)" '(1 1))
    "@>")

(is (multiple-value-list
     (yield (make-op :as
                         (make-sql-symbol "table-name")
                         (make-sql-symbol "a"))))
    (list "`table-name` AS `a`" nil)
    "AS")

(is (multiple-value-list
     (yield (make-op :in
                     (make-sql-symbol "a")
                     (list
                      (make-sql-variable 1)
                      (make-sql-variable 10)
                      (make-sql-variable 100)))))
    (list "(`a` IN (?, ?, ?))" '(1 10 100))
    "IN")
(is (multiple-value-list
     (yield (make-op :in
                     (make-sql-symbol "a")
                     (list
                      (make-sql-variable 1)
                      (make-sql-variable 10)
                      (make-op :*
                               (make-sql-variable 10)
                               (make-sql-variable 10))))))
    (list "(`a` IN (?, ?, (? * ?)))" '(1 10 10 10))
    "IN")
(is (multiple-value-list
     (yield (make-op :not-in
                         (make-sql-symbol "a")
                         (list (make-sql-variable 1)
                               (make-sql-variable 10)
                               (make-sql-variable 100)))))
    (list "(`a` NOT IN (?, ?, ?))" '(1 10 100))
    "NOT IN")
(is-error (make-op :in
                   (make-sql-symbol "a")
                   (make-sql-variable 1))
          type-error)
(is-error (make-op :not-in
                   (make-sql-symbol "a")
                   (make-sql-variable 1))
          type-error)

(is (multiple-value-list
     (yield (make-op :like
                         (make-sql-symbol "name")
                         (make-sql-variable "John %"))))
    (list "(`name` LIKE ?)" '("John %"))
    "LIKE")

(is-error (make-op :in
                   (make-sql-symbol "a")
                   (make-sql-variable 1)
                   (make-sql-variable "extra argument"))
          'program-error)

(is (multiple-value-list
     (yield (make-op :is-distinct-from
                         (make-sql-variable 1 )
                         (make-sql-variable 1))))
    (list "(? IS DISTINCT FROM ?)" '(1 1))
    "IS DISTINCT FROM")

(is (multiple-value-list
     (yield (make-op :is-not-distinct-from
                         (make-sql-variable 1 )
                         (make-sql-variable 1))))
    (list "(? IS NOT DISTINCT FROM ?)" '(1 1))
    "IS NOT DISTINCT FROM")

(diag "conjunctive-op")

(is (multiple-value-list
     (yield (make-op :or
                     (make-sql-variable 1)
                     (make-sql-variable 2)
                     (make-sql-variable 3))))
    (list "(? OR ? OR ?)" '(1 2 3)))
(is (multiple-value-list
     (yield (make-op :or
                     (make-op :>
                              (make-sql-symbol "age")
                              (make-sql-variable 65))
                     (make-op :<=
                              (make-sql-symbol "age")
                              (make-sql-variable 18)))))
    (list "((`age` > ?) OR (`age` <= ?))" '(65 18)))
(is (multiple-value-list
     (yield (make-op :and
                     (make-op :>
                              (make-sql-symbol "age")
                              (make-sql-variable 15))
                     (make-op :>
                              (make-sql-variable 21)
                              (make-sql-symbol "age"))
                     (make-op :like
                              (make-sql-symbol "name")
                              (make-sql-variable "John %")))))
    (list "((`age` > ?) AND (? > `age`) AND (`name` LIKE ?))"
          '(15 21 "John %")))
(is (multiple-value-list
     (yield (make-op :+
                     (make-sql-variable 1)
                     (make-sql-variable 3)
                     (make-op :*
                              (make-sql-variable 100)
                              (make-op :-
                                       (make-sql-variable 0.8)
                                       (make-sql-variable 0.3)))
                     (make-op :/
                              (make-sql-symbol "a")
                              (make-sql-variable 10))
                     (make-op :%
                              (make-sql-variable 100)
                              (make-sql-variable 3)
                              (make-sql-variable 3)))))
    (list "(? + ? + (? * (? - ?)) + (`a` / ?) + (? % ? % ?))"
          '(1 3 100 0.8 0.3 10 100 3 3)))

(is (multiple-value-list
     (yield (make-op :union
                     (select :* (from :table1))
                     (select :* (from :table2))
                     (order-by :column1)
                     (limit 1))))
    (list "(SELECT * FROM `table1`) UNION (SELECT * FROM `table2`) ORDER BY `column1` LIMIT 1"
          nil))

(let ((*inside-select* t))
  (is (multiple-value-list
       (yield (make-op :union
                       (select :* (from :table1))
                       (select :* (from :table2))
                       (order-by :column1)
                       (limit 1))))
      (list "((SELECT * FROM `table1`) UNION (SELECT * FROM `table2`) ORDER BY `column1` LIMIT 1)"
            nil)))

(is (multiple-value-list
     (yield (make-op :union-all
                     (select :* (from :table1))
                     (select :* (from :table2))
                     (order-by :column1)
                     (limit 1))))
    (list "(SELECT * FROM `table1`) UNION ALL (SELECT * FROM `table2`) ORDER BY `column1` LIMIT 1"
          nil))

(let ((*inside-select* t))
  (is (multiple-value-list
       (yield (make-op :union-all
                       (select :* (from :table1))
                       (select :* (from :table2))
                       (order-by :column1)
                       (limit 1))))
      (list "((SELECT * FROM `table1`) UNION ALL (SELECT * FROM `table2`) ORDER BY `column1` LIMIT 1)"
            nil)))

(diag "function-op")

(ok (make-op :count :*))
(is (slot-value (make-op :count :*) 'name) "COUNT")
(is-type (make-op :count :*) 'function-op)
(is (yield (make-op :count :*)) "COUNT(*)")
(is (yield (make-op :count :column)) "COUNT(`column`)")

(diag "sql-compile op")

(ok (sql-compile (make-op :+ 1 2 3)))

(let ((op (sql-compile (make-op :+ 1 2 3))))
  (ok (make-op :* op (make-op :- 10 2) op))
  (is (multiple-value-list
       (yield (make-op :* op (make-op :- 10 2) op)))
      '("((? + ? + ?) * (? - ?) * (? + ? + ?))" (1 2 3 10 2 1 2 3))))

(diag "subquery")
(is (multiple-value-list
     (yield (make-op :=
                     :id
                     (select :id (from :users)))))
    (list "(`id` = (SELECT `id` FROM `users`))" NIL)
    "=")

(is (multiple-value-list
     (yield (make-op :!=
                     :id
                     (select :id (from :users)))))
    (list "(`id` != (SELECT `id` FROM `users`))" NIL)
    "=")

(is (multiple-value-list
     (yield (make-op :in
                     :id
                     (select :id (from :users)))))
    (list "(`id` IN (SELECT `id` FROM `users`))" NIL)
    "IN")

(is (multiple-value-list
     (yield (make-op :not-in
                     :id
                     (select :id (from :users)))))
    (list "(`id` NOT IN (SELECT `id` FROM `users`))" NIL)
    "NOT IN")

(finalize)

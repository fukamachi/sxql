(defpackage #:sxql/test/operator
  (:nicknames #:t.sxql.operator)
  (:use #:cl
        #:sxql
        #:sxql/sql-type
        #:sxql/operator
        #:rove)
  (:shadowing-import-from #:sxql/test/prepare
                          #:is-error
                          #:is-mv))
(in-package #:sxql/test/operator)

(setup
  (setf *quote-character* #\`))

(teardown
  (setf *quote-character* nil))

(deftest operator-creation-tests
  (testing "unary operator creation"
    (ok (make-unary-op "NOT" (make-sql-variable 1)))
    (ok (make-unary-op "NOT" (make-unary-op "NOT" (make-sql-variable 1))))
    (is-error (make-unary-op "NOT" 1) type-error))

  (testing "infix operator creation"
    (ok (make-infix-op "=" (make-sql-variable 1) (make-sql-variable 1)))
    (ok (make-infix-op "="
                       (make-unary-op "NOT" (make-sql-variable 1))
                       (make-sql-variable 1)))
    (is-error (make-infix-op "=" 1 (make-sql-variable 1)) type-error))

  (testing "conjunctive operator creation"
    (is-error (make-conjunctive-op "+" 1) type-error)
    (ok (make-conjunctive-op "+" (make-sql-variable 1)))
    (ok (make-conjunctive-op "+"
                             (make-sql-variable 1)
                             (make-sql-variable 3)
                             (make-sql-list (make-sql-symbol "a")
                                            (make-sql-symbol "b"))
                             (make-sql-keyword "NULL")))))

(deftest keyword-conversion-tests
  (testing "SQL keyword conversion"
    (loop for (k . s) in '((:null . "NULL")
                           (:current_date . "CURRENT_DATE")
                           (:current_time . "CURRENT_TIME")
                           (:current_timestamp . "CURRENT_TIMESTAMP"))
          do (is-mv (convert-for-sql k) (list s nil)))))

(deftest unary-operator-tests
  (testing "basic unary operators"
    (is-mv (make-op :not (make-sql-variable 1))
           (list "(NOT ?)" '(1)))
    (is-mv (make-op :is-null (make-sql-symbol "a"))
           (list "(`a` IS NULL)" nil))
    (is-mv (make-op :not-null (make-sql-symbol "a"))
           (list "(`a` IS NOT NULL)" nil)))

  (testing "raw SQL operator"
    (ok (make-op :raw (make-sql-variable "SELECT * FROM table")))
    (ok (make-op :raw "SELECT * FROM table"))
    (is-mv (make-op :raw "SELECT * FROM table")
           (list "(SELECT * FROM table)" nil)))

  (testing "unary operator error conditions"
    (is-error (make-op :not
                       (make-sql-symbol "a")
                       (make-sql-variable 1))
              program-error))

  (testing "DISTINCT operator"
    (is-mv (make-op :distinct :age)
           (list "DISTINCT `age`" nil))))

(deftest comparison-operator-tests
  (testing "basic comparison operators"
    (is-mv (make-op :=
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? = ?)" '(1 1)))

    (is-mv (make-op :!=
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? != ?)" '(1 1)))

    (is-mv (make-op :<
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? < ?)" '(1 1)))

    (is-mv (make-op :>
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? > ?)" '(1 1)))

    (is-mv (make-op :>=
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? >= ?)" '(1 1)))

    (is-mv (make-op :<=
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? <= ?)" '(1 1))))

  (testing "advanced comparison operators"
    (is-mv (make-op :a<
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? @< ?)" '(1 1)))

    (is-mv (make-op :a>
                    (make-sql-variable 1)
                    (make-sql-variable 1))
           (list "(? @> ?)" '(1 1)))))

(deftest special-infix-operator-tests
  (testing "AS operator"
    (is-mv (make-op :as
                    (make-sql-symbol "table-name")
                    (make-sql-symbol "a"))
           (list "`table-name` AS `a`" nil)))

  (testing "IN operator"
    (is-mv (make-op :in
                    (make-sql-symbol "a")
                    (list
                     (make-sql-variable 1)
                     (make-sql-variable 10)
                     (make-sql-variable 100)))
           (list "(`a` IN (?, ?, ?))" '(1 10 100)))

    (is-mv (make-op :in
                    (make-sql-symbol "a")
                    (list
                     (make-sql-variable 1)
                     (make-sql-variable 10)
                     (make-op :*
                              (make-sql-variable 10)
                              (make-sql-variable 10))))
           (list "(`a` IN (?, ?, (? * ?)))" '(1 10 10 10)))

    (is-mv (make-op :not-in
                    (make-sql-symbol "a")
                    (list (make-sql-variable 1)
                          (make-sql-variable 10)
                          (make-sql-variable 100)))
           (list "(`a` NOT IN (?, ?, ?))" '(1 10 100)))

    (is-error (make-op :in
                       (make-sql-symbol "a")
                       (make-sql-variable 1))
              type-error)
    (is-error (make-op :not-in
                       (make-sql-symbol "a")
                       (make-sql-variable 1))
              type-error))

  (testing "LIKE operator"
    (is-mv (make-op :like
                    (make-sql-symbol "name")
                    (make-sql-variable "John %"))
           (list "(`name` LIKE ?)" '("John %"))))

  (testing "operator argument validation"
    (is-error (make-op :in
                       (make-sql-symbol "a")
                       (make-sql-variable 1)
                       (make-sql-variable "extra argument"))
              program-error))

  (testing "DISTINCT operators"
    (is-mv (make-op :is-distinct-from
                    (make-sql-variable 1 )
                    (make-sql-variable 1))
           (list "(? IS DISTINCT FROM ?)" '(1 1)))

    (is-mv (make-op :is-not-distinct-from
                    (make-sql-variable 1 )
                    (make-sql-variable 1))
           (list "(? IS NOT DISTINCT FROM ?)" '(1 1)))))

(deftest conjunctive-operator-tests
  (testing "logical operators"
    (is-mv (make-op :or
                    (make-sql-variable 1)
                    (make-sql-variable 2)
                    (make-sql-variable 3))
           (list "(? OR ? OR ?)" '(1 2 3)))

    (is-mv (make-op :or
                    (make-op :>
                             (make-sql-symbol "age")
                             (make-sql-variable 65))
                    (make-op :<=
                             (make-sql-symbol "age")
                             (make-sql-variable 18)))
           (list "((`age` > ?) OR (`age` <= ?))" '(65 18)))

    (is-mv (make-op :and
                    (make-op :>
                             (make-sql-symbol "age")
                             (make-sql-variable 15))
                    (make-op :>
                             (make-sql-variable 21)
                             (make-sql-symbol "age"))
                    (make-op :like
                             (make-sql-symbol "name")
                             (make-sql-variable "John %")))
           (list "((`age` > ?) AND (? > `age`) AND (`name` LIKE ?))"
                 '(15 21 "John %"))))

  (testing "arithmetic operators"
    (is-mv (make-op :+
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
                             (make-sql-variable 3)))
           (list "(? + ? + (? * (? - ?)) + (`a` / ?) + (? % ? % ?))"
                 '(1 3 100 0.8 0.3 10 100 3 3)))))

(deftest union-operator-tests
  (testing "UNION operator"
    (is-mv (make-op :union
                    (select :* (from :table1))
                    (select :* (from :table2))
                    (order-by :column1)
                    (limit 1))
           (list "SELECT * FROM `table1` UNION SELECT * FROM `table2` ORDER BY `column1` LIMIT 1"
                 nil))

    (let ((*inside-select* t))
      (is-mv (make-op :union
                      (select :* (from :table1))
                      (select :* (from :table2))
                      (order-by :column1)
                      (limit 1))
             (list "(SELECT * FROM `table1` UNION SELECT * FROM `table2` ORDER BY `column1` LIMIT 1)"
                   nil))))

  (testing "UNION ALL operator"
    (is-mv (make-op :union-all
                    (select :* (from :table1))
                    (select :* (from :table2))
                    (order-by :column1)
                    (limit 1))
           (list "SELECT * FROM `table1` UNION ALL SELECT * FROM `table2` ORDER BY `column1` LIMIT 1"
                 nil))

    (let ((*inside-select* t))
      (is-mv (make-op :union-all
                      (select :* (from :table1))
                      (select :* (from :table2))
                      (order-by :column1)
                      (limit 1))
             (list "(SELECT * FROM `table1` UNION ALL SELECT * FROM `table2` ORDER BY `column1` LIMIT 1)"
                   nil)))))

(deftest case-operator-tests
  (testing "CASE WHEN statements"
    (is-mv (make-op :case
                    (make-op :when
                             (make-op :=
                                      (make-sql-symbol "a")
                                      (make-sql-variable 0))
                             (make-sql-variable "zero"))
                    (make-op :when
                             (make-op :=
                                      (make-sql-symbol "a")
                                      (make-sql-variable 1))
                             (make-sql-variable "one"))
                    (make-op :else
                             (make-sql-variable "other")))
           (list "CASE WHEN (`a` = ?) THEN ? WHEN (`a` = ?) THEN ? ELSE ? END"
                 '(0 "zero" 1 "one" "other"))))

  (testing "CASE expression statements"
    (is-mv (make-op :case
                    (make-sql-symbol "a")
                    (make-op :when
                             (make-sql-variable 0)
                             (make-sql-variable "zero"))
                    (make-op :when
                             (make-sql-variable 1)
                             (make-sql-variable "one"))
                    (make-op :else
                             (make-sql-variable "other")))
           (list "CASE `a` WHEN ? THEN ? WHEN ? THEN ? ELSE ? END"
                 '(0 "zero" 1 "one" "other")))))

(deftest function-operator-tests
  (testing "COUNT function"
    (ok (make-op :count :*))
    (ok (equal (slot-value (make-op :count :*) 'name) "COUNT"))
    (ok (typep (make-op :count :*) 'function-op))
    (ok (equal (yield (make-op :count :*)) "COUNT(*)"))
    (ok (equal (yield (make-op :count :column)) "COUNT(`column`)"))))

(deftest sql-compile-operator-tests
  (testing "sql-compile with operators"
    (ok (sql-compile (make-op :+ 1 2 3)))

    (let ((op (sql-compile (make-op :+ 1 2 3))))
      (ok (make-op :* op (make-op :- 10 2) op))
      (is-mv (make-op :* op (make-op :- 10 2) op)
             '("((? + ? + ?) * (? - ?) * (? + ? + ?))" (1 2 3 10 2 1 2 3))))))

(deftest subquery-operator-tests
  (testing "operators with subqueries"
    (is-mv (make-op :=
                    :id
                    (select :id (from :users)))
           (list "(`id` = (SELECT `id` FROM `users`))" NIL))

    (is-mv (make-op :!=
                    :id
                    (select :id (from :users)))
           (list "(`id` != (SELECT `id` FROM `users`))" NIL))

    (is-mv (make-op :in
                    :id
                    (select :id (from :users)))
           (list "(`id` IN (SELECT `id` FROM `users`))" NIL))

    (is-mv (make-op :not-in
                    :id
                    (select :id (from :users)))
           (list "(`id` NOT IN (SELECT `id` FROM `users`))" NIL))))

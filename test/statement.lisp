(defpackage #:sxql/test/statement
  (:nicknames #:t.sxql.statement)
  (:use #:cl
        #:sxql
        #:sxql/sql-type
        #:sxql/operator
        #:sxql/clause
        #:sxql/statement
        #:rove)
  (:shadowing-import-from #:sxql/test/prepare
                          #:is-error
                          #:is-mv))
(in-package #:sxql/test/statement)

(setup
  (setf *quote-character* #\`))

(teardown
  (setf *quote-character* nil))

(deftest select-statement-tests
  (testing "basic SELECT statement creation"
    (ok (make-statement :select
                        (make-clause :fields :*)
                        (make-clause :from (make-sql-symbol "table-name"))
                        (make-clause :where
                                     (make-op := :name "Eitaro")))))

  (testing "SELECT statement SQL generation"
    (ok (equal (yield (make-statement :select
                                      (make-clause :fields :*)
                                      (make-clause :from (make-sql-symbol "table-name"))
                                      (make-clause :where
                                                   (make-op := :name "Eitaro"))))
               "SELECT * FROM `table-name` WHERE (`name` = ?)"))

    (ok (equal (yield (make-statement :select
                                      (make-clause :fields :a :b)
                                      (make-clause :from (make-sql-symbol "table-name"))
                                      (make-clause :where
                                                   (make-op := :name "Eitaro"))))
               "SELECT `a`, `b` FROM `table-name` WHERE (`name` = ?)"))

    (is-mv (make-statement :select (make-clause :fields (make-op :+ 1 1)))
           (list "SELECT (? + ?)" '(1 1)))

    (is-mv (make-statement :select
                           (make-clause :fields :a 1)
                           (make-clause :from
                                        (make-sql-symbol "table"))
                           (make-clause :order-by
                                        (make-sql-symbol "a")))
           (list "SELECT `a`, ? FROM `table` ORDER BY `a`" '(1)))))

(deftest select-advanced-tests
  (testing "SELECT with FOR UPDATE clause"
    (ok (make-statement :select
                        (make-clause :fields :a)
                        (make-clause :from (make-sql-symbol "table"))
                        (make-clause :order-by (make-sql-symbol "a"))
                        (make-clause :updatability :update :of (make-sql-symbol "a"))))

    (is-mv (make-statement :select
                           (make-clause :fields :a)
                           (make-clause :from (make-sql-symbol "table"))
                           (make-clause :order-by (make-sql-symbol "a"))
                           (make-clause :updatability :update :of (make-sql-symbol "a") :nowait t))
           (list "SELECT `a` FROM `table` ORDER BY `a` FOR UPDATE OF `a` NOWAIT" nil)))

  (testing "SELECT with subquery"
    (is-mv (make-statement :select
                           (make-clause :fields :*)
                           (make-clause :from
                                        (make-sql-symbol "table"))
                           (make-clause :where
                                        (make-op :>
                                                 :max
                                                 (make-op :ifnull
                                                          (make-statement :select
                                                                          (make-clause :fields :count)
                                                                          (make-clause :from
                                                                                       (make-sql-symbol "table")))
                                                          0))))
           (list "SELECT * FROM `table` WHERE (`max` > IFNULL((SELECT `count` FROM `table`), ?))" '(0)))))

(deftest insert-statement-tests
  (testing "INSERT with SET clause"
    (is-mv (make-statement :insert-into (make-sql-symbol "table")
                           (make-clause :set=
                                        (make-sql-symbol "a")
                                        (make-sql-variable 10)))
           '("INSERT INTO `table` (`a`) VALUES (?)" (10))))

  (testing "INSERT with column list and values"
    (is-mv (make-statement :insert-into (make-sql-symbol "table")
                           (list :col1 :col2)
                           (list 1 2))
           '("INSERT INTO `table` (`col1`, `col2`) VALUES (?, ?)" (1 2))))

  (testing "INSERT with multiple value rows"
    (is-mv (make-statement :insert-into (make-sql-symbol "table")
                           (list :col1 :col2)
                           (list (list 1 2) (list 3 4)))
           '("INSERT INTO `table` (`col1`, `col2`) VALUES (?, ?), (?, ?)" (1 2 3 4)))))

(deftest update-statement-tests
  (testing "UPDATE with multiple clauses"
    (is-mv (make-statement :update (make-sql-symbol "table")
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
                           (make-clause :limit 5))
           '("UPDATE `table` SET `a` = ?, `b` = ? WHERE (`age` > ?) ORDER BY `id` LIMIT 5" (10 20 20)))))

(deftest create-table-tests
  (testing "CREATE TABLE basic syntax"
    (is-mv (make-statement :create-table
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
                                                :unique t)))
           '("CREATE TABLE `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None"))))

  (testing "CREATE TABLE with quoted table name"
    (is-mv (make-statement :create-table
                           '(:enemy)
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
                                                :unique t)))
           '("CREATE TABLE `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None"))))

  (testing "CREATE TABLE IF NOT EXISTS"
    (is-mv (make-statement :create-table
                           '(:enemy :if-not-exists t)
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
                                                :unique t)))
           '("CREATE TABLE IF NOT EXISTS `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None")))))

(deftest alter-table-tests
  (testing "ALTER TABLE with multiple ADD COLUMN clauses"
    (is-mv (make-statement :alter-table :tweet
                           (make-clause :add-column :id
                                        :type '(:bigint 20 :unsigned)
                                        :primary-key t
                                        :auto-increment t
                                        :first t)
                           (make-clause :add-column :updated_at
                                        :type 'timestamp))
           '("ALTER TABLE `tweet` ADD COLUMN `id` BIGINT(20) UNSIGNED AUTO_INCREMENT PRIMARY KEY FIRST, ADD COLUMN `updated_at` TIMESTAMP" nil)))

  (testing "ALTER TABLE with ENUM type"
    (is-mv (make-statement :alter-table :tweet
                           (make-clause :add-column :status
                                        :type '(:enum ("temporary" "registered" "banned"))))
           '("ALTER TABLE `tweet` ADD COLUMN `status` ENUM('temporary', 'registered', 'banned')" nil))))

(deftest ddl-statement-tests
  (testing "CREATE INDEX statement"
    (is-mv (make-statement :create-index :index_name
                           :unique t
                           :using :btree
                           :on '(:table :column1 :column2))
           '("CREATE UNIQUE INDEX `index_name` USING BTREE ON `table` (`column1`, `column2`)" nil)))

  (testing "DROP INDEX statement"
    (is-mv (make-statement :drop-index "index_name" :if-exists t)
           '("DROP INDEX IF EXISTS `index_name`" nil)))

  (testing "EXPLAIN statement"
    (is-mv (make-statement :explain
                           (make-statement :select
                                           (make-clause :fields :*)
                                           (make-clause :from (make-sql-symbol "table-name")))
                           :analyze t
                           :verbose t)
           '("EXPLAIN ANALYZE VERBOSE SELECT * FROM `table-name`" nil))))

(deftest sql-compile-statement-tests
  (testing "sql-compile with SELECT statement"
    (let ((stmt (sql-compile
                 (make-statement :select
                                 (make-clause :fields :*)
                                 (make-clause :from (make-sql-symbol "table-name"))
                                 (make-clause :where
                                              (make-op :<
                                                       (make-sql-symbol "age")
                                                       (make-sql-variable 20)))))))
      (ok stmt)

      (is-mv stmt
             '("SELECT * FROM `table-name` WHERE (`age` < ?)" (20)))

      (testing "UNION with compiled statement"
        (let ((union-stmt
                (make-op :union stmt (make-statement :select
                                                     (make-clause :fields :*)
                                                     (make-clause :from (make-sql-symbol "table-2"))))))
          (ok union-stmt)
          (is-mv union-stmt
                 '("SELECT * FROM `table-name` WHERE (`age` < ?) UNION SELECT * FROM `table-2`" (20))))))))

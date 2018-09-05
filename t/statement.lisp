(in-package :cl-user)
(defpackage t.sxql.statement
  (:use :cl
        :sxql
        :sxql.sql-type
        :sxql.operator
        :sxql.clause
        :sxql.statement
        :prove)
  (:shadowing-import-from :t.sxql.prepare
                          :is-error))
(in-package :t.sxql.statement)

(plan 19)

(diag "statement")

(ok (make-statement :select
                    (make-clause :fields :*)
                    (make-clause :from (make-sql-symbol "table-name"))
                    (make-clause :where
                                 (make-op := :name "Eitaro"))))

(is (yield (make-statement :select
                           (make-clause :fields :*)
                           (make-clause :from (make-sql-symbol "table-name"))
                           (make-clause :where
                                        (make-op := :name "Eitaro"))))
    "SELECT * FROM `table-name` WHERE (`name` = ?)")

(is (yield (make-statement :select
                           (make-clause :fields :a :b)
                           (make-clause :from (make-sql-symbol "table-name"))
                           (make-clause :where
                                        (make-op := :name "Eitaro"))))
    "SELECT `a`, `b` FROM `table-name` WHERE (`name` = ?)")

(is (multiple-value-list
     (yield (make-statement :select (make-clause :fields (make-op :+ 1 1)))))
    (list "SELECT (? + ?)" '(1 1)))

(is (multiple-value-list
     (yield (make-statement :select
                            (make-clause :fields :a 1)
                            (make-clause :from
                                         (make-sql-symbol "table"))
                            (make-clause :order-by
                                         (make-sql-symbol "a")))))
    (list "SELECT `a`, ? FROM `table` ORDER BY `a`" '(1)))

(ok (make-statement :select
                    (make-clause :fields :a)
                    (make-clause :from (make-sql-symbol "table"))
                    (make-clause :order-by (make-sql-symbol "a"))
                    (make-clause :updatability :update :of (make-sql-symbol "a"))))
(is (multiple-value-list
     (yield (make-statement :select
                            (make-clause :fields :a)
                            (make-clause :from (make-sql-symbol "table"))
                            (make-clause :order-by (make-sql-symbol "a"))
                            (make-clause :updatability :update :of (make-sql-symbol "a") :nowait t))))
    (list "SELECT `a` FROM `table` ORDER BY `a` FOR UPDATE OF `a` NOWAIT" nil))

(is (multiple-value-list
     (yield (make-statement :select
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
                                                           0))))))
    (list "SELECT * FROM `table` WHERE (`max` > IFNULL((SELECT `count` FROM `table`), ?))" '(0)))

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
    '("CREATE TABLE `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None"))
    "CREATE TABLE")

(is (multiple-value-list
     (yield (make-statement :create-table
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
                                  :unique t)))))
    '("CREATE TABLE `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None"))
    "CREATE TABLE")

(is (multiple-value-list
     (yield (make-statement :create-table
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
                                  :unique t)))))
    '("CREATE TABLE IF NOT EXISTS `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None"))
    "CREATE TABLE IF NOT EXISTS")

(is (multiple-value-list
     (yield (make-statement :alter-table :tweet
              (make-clause :add-column :id
                           :type '(:bigint 20 :unsigned)
                           :primary-key t
                           :auto-increment t
                           :first t)
              (make-clause :add-column :updated_at
                           :type 'timestamp))))
    '("ALTER TABLE `tweet` ADD COLUMN `id` BIGINT(20) UNSIGNED AUTO_INCREMENT PRIMARY KEY FIRST, ADD COLUMN `updated_at` TIMESTAMP" nil))

(is (multiple-value-list
     (yield (make-statement :alter-table :tweet
              (make-clause :add-column :status
                           :type '(:enum ("temporary" "registered" "banned"))))))
    '("ALTER TABLE `tweet` ADD COLUMN `status` ENUM('temporary', 'registered', 'banned')" nil))

(is (multiple-value-list
     (yield (make-statement :create-index :index_name
                            :unique t
                            :using :btree
                            :on '(:table :column1 :column2))))
    '("CREATE UNIQUE INDEX `index_name` USING BTREE ON `table` (`column1`, `column2`)" nil)
    "CREATE UNIQUE INDEX")

(is (multiple-value-list
     (yield (make-statement :drop-index "index_name" :if-exists t)))
    '("DROP INDEX IF EXISTS `index_name`" nil)
    "DROP INDEX")

(diag "sql-compile statement")

(let ((stmt (sql-compile
             (make-statement :select
                             (make-clause :fields :*)
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
                                               (make-clause :fields :*)
                                               (make-clause :from (make-sql-symbol "table-2"))))))
    (ok union-stmt)
    (is (multiple-value-list (yield union-stmt))
        '("(SELECT * FROM `table-name` WHERE (`age` < ?)) UNION (SELECT * FROM `table-2`)" (20)))))

(finalize)

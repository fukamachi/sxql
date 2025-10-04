(defpackage #:sxql/test/clause
  (:nicknames #:t.sxql.clause)
  (:use #:cl
        #:rove
        #:sxql/sql-type
        #:sxql/operator
        #:sxql/clause
        #:sxql/compile)
  (:shadowing-import-from #:sxql/test/prepare
                          #:is-error
                          #:is-mv))
(in-package #:sxql/test/clause)

(setup
  (setf *quote-character* #\`))

(teardown
  (setf *quote-character* nil))

(deftest basic-clause-tests
  (testing "WHERE clause"
    (ok (make-clause :where (make-op := :a 10)))
    (is-mv (make-clause :where (make-op := :a 10))
           (list "WHERE (`a` = ?)" '(10)))
    (is-error (make-clause :where
                           (make-op := :a 10)
                           (make-op :!= :b 20))
              program-error))

  (testing "FROM clause"
    (ok (make-clause :from (make-sql-symbol "table-name")))
    (ok (make-clause :from (make-op :as :table-name :a)))
    (is-mv (make-clause :from (make-sql-symbol "table-name"))
           (list "FROM `table-name`" nil))
    (is-mv (make-clause :from
                        (make-op :as :table-name :a))
           (list "FROM `table-name` AS `a`" nil))))

(deftest order-by-clause-tests
  (testing "ORDER BY clause creation and generation"
    (ok (make-clause :order-by (make-sql-symbol "a")))
    (is-mv (make-clause :order-by (make-sql-symbol "a"))
           (list "ORDER BY `a`" nil))
    (is-mv (make-clause :order-by
                        (make-sql-list
                         (make-sql-symbol "a")
                         (make-sql-symbol "b")))
           (list "ORDER BY (`a`, `b`)" nil))
    (is-mv (make-clause :order-by
                        (make-sql-list
                         (make-op :desc (make-sql-symbol "a"))
                         (make-sql-symbol "b")))
           (list "ORDER BY (`a` DESC, `b`)" nil))))

(deftest group-by-clause-tests
  (testing "GROUP BY clause creation"
    (ok (make-clause :group-by (make-sql-symbol "a")))
    (ok (make-clause :group-by
                     (make-sql-list
                      (make-sql-symbol "a")
                      (make-sql-symbol "b"))))
    (ok (make-clause :group-by (make-op :+
                                        (make-sql-symbol "a")
                                        (make-sql-variable 1)))))

  (testing "GROUP BY clause SQL generation"
    (is-mv (make-clause :group-by (make-sql-symbol "a"))
           (list "GROUP BY `a`" nil))
    (is-mv (make-clause :group-by
                        (make-sql-list
                         (make-sql-symbol "a")
                         (make-sql-symbol "b")))
           (list "GROUP BY (`a`, `b`)" nil))
    (is-mv (make-clause :group-by
                        (make-op :+ (make-sql-symbol "a") (make-sql-variable 1)))
           (list "GROUP BY (`a` + ?)" '(1)))))

(deftest having-returning-clause-tests
  (testing "HAVING clause"
    (is-mv (make-clause :having
                        (make-op :>= (make-sql-symbol "hoge") (make-sql-variable 88)))
           (list "HAVING (`hoge` >= ?)" '(88))))

  (testing "RETURNING clause"
    (is-mv (make-clause :returning (make-sql-symbol "id"))
           (list "RETURNING `id`" nil))
    (is-mv (make-clause :returning (make-sql-symbol "id") (make-sql-symbol "name"))
           (list "RETURNING `id`, `name`" nil))))

(deftest updatability-clause-tests
  (testing "FOR UPDATE clause creation"
    (ok (make-clause :updatability :update))
    (ok (make-clause :updatability :update :of '(:hoge :piyo)))
    (ok (make-clause :updatability :update :of '(:hoge :piyo) :nowait t))
    (ok (make-clause :updatability :update :of '(:hoge :piyo) :skip-locked t)))

  (testing "FOR UPDATE clause SQL generation"
    (is-mv (make-clause :updatability :update)
           (list "FOR UPDATE" nil))
    (is-mv (make-clause :updatability :share)
           (list "FOR SHARE" nil))
    (is-mv (make-clause :updatability :update :of '(:hoge :fuga.piyo))
           (list "FOR UPDATE OF `hoge`, `fuga`.`piyo`" nil))
    (is-mv (make-clause :updatability :update :of '(:hoge :piyo) :nowait t)
           (list "FOR UPDATE OF `hoge`, `piyo` NOWAIT" nil))
    (is-mv (make-clause :updatability :update :nowait t)
           (list "FOR UPDATE NOWAIT" nil))
    (is-mv (make-clause :updatability :update :of '(:hoge :piyo) :skip-locked t)
           (list "FOR UPDATE OF `hoge`, `piyo` SKIP LOCKED" nil))
    (is-mv (make-clause :updatability :update :skip-locked t)
           (list "FOR UPDATE SKIP LOCKED" nil))))

(deftest limit-offset-clause-tests
  (testing "LIMIT clause"
    (ok (make-clause :limit (make-sql-variable 1)))
    (ok (make-clause :limit (make-sql-variable 0) (make-sql-variable 10)))
    (is-mv (make-clause :limit (make-sql-variable 1))
           (list "LIMIT 1" nil))
    (is-mv (make-clause :limit
                        (make-sql-variable 0)
                        (make-sql-variable 10))
           (list "LIMIT 0, 10" nil))
    (is-error (make-clause :limit (make-sql-symbol "a")) type-error)
    (is-error (make-clause :limit
                           (make-sql-variable 1)
                           (make-sql-variable 2)
                           (make-sql-variable 2))
              program-error))

  (testing "OFFSET clause"
    (ok (make-clause :offset (make-sql-variable 1)))
    (is-mv (make-clause :offset (make-sql-variable 1000))
           (list "OFFSET 1000" nil))
    (is-error (make-clause :offset
                           (make-sql-variable 1)
                           (make-sql-variable 2))
              program-error)))

(deftest set-clause-tests
  (testing "SET clause creation and generation"
    (ok (make-clause :set= :a 1))
    (ok (make-clause :set= :a 1 :b 2))
    (is-mv (make-clause :set= :a 1 :b 2)
           (list "SET `a` = ?, `b` = ?" '(1 2)))
    (ok (make-clause :set= :a nil))))

(deftest key-clause-tests
  (testing "PRIMARY KEY clauses"
    (is-mv (make-clause :primary-key '(:id))
           (list "PRIMARY KEY (`id`)" nil))
    (is-mv (make-clause :primary-key :id)
           (list "PRIMARY KEY (`id`)" nil))
    (is-mv (make-clause :primary-key "primary_key_is_id"'(:id))
           (list "PRIMARY KEY 'primary_key_is_id' (`id`)" nil)))

  (testing "UNIQUE KEY clauses"
    (is-mv (make-clause :unique-key '(:name :country))
           (list "UNIQUE (`name`, `country`)" nil))
    (is-mv (make-clause :unique-key "name_and_country_index" '(:name :country))
           (list "UNIQUE 'name_and_country_index' (`name`, `country`)" nil)))

  (testing "KEY clauses"
    (is-mv (make-clause :key '(:id))
           (list "KEY (`id`)" nil))
    (is-mv (make-clause :key "id_is_unique" '(:id))
           (list "KEY 'id_is_unique' (`id`)" nil))))

(deftest foreign-key-clause-tests
  (testing "FOREIGN KEY references clause creation"
    (ok (sxql.clause::make-references-clause
         (sxql.sql-type:make-sql-symbol "project")
         (sxql.sql-type:make-sql-list (sxql.sql-type:make-sql-symbol "id")))))

  (testing "FOREIGN KEY clause generation"
    (is-mv (make-clause :foreign-key '(:project_id) :references '(:project :id))
           (list "FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)" nil))))

(deftest column-type-tests
  (testing "SQL column type generation from lists"
    (ok (equal (yield (sxql.clause::make-sql-column-type-from-list '(:integer)))
               "INTEGER"))
    (ok (equal (yield (sxql.clause::make-sql-column-type-from-list '(:integer 11)))
               "INTEGER(11)"))
    (ok (equal (yield (sxql.clause::make-sql-column-type-from-list '(:integer 11 :unsigned)))
               "INTEGER(11) UNSIGNED"))
    (ok (equal (yield (sxql.clause::make-sql-column-type-from-list '(:integer nil :unsigned)))
               "INTEGER UNSIGNED"))))

(deftest column-modification-clause-tests
  (testing "ADD COLUMN clause"
    (is-mv (make-clause :add-column :updated_at
                        :type 'integer
                        :default 0
                        :not-null t
                        :after :created_at)
           (list "ADD COLUMN `updated_at` INTEGER NOT NULL DEFAULT ? AFTER `created_at`"
                 '(0))))

  (testing "MODIFY COLUMN clause"
    (is-mv (make-clause :modify-column
                        :updated_at
                        :type 'datetime
                        :not-null t)
           (list "MODIFY COLUMN `updated_at` DATETIME NOT NULL" nil)))

  (testing "ALTER COLUMN clauses"
    (is-mv (make-clause :alter-column :user :type '(:varchar 64))
           (list "ALTER COLUMN `user` TYPE VARCHAR(64)" nil))
    (is-mv (make-clause :alter-column :id :set-default 1)
           (list "ALTER COLUMN `id` SET DEFAULT ?" '(1)))
    (is-mv (make-clause :alter-column :id :drop-default t)
           (list "ALTER COLUMN `id` DROP DEFAULT" nil))
    (is-mv (make-clause :alter-column :profile :not-null t)
           (list "ALTER COLUMN `profile` SET NOT NULL" nil)))

  (testing "DROP COLUMN and RENAME TO clauses"
    (is-mv (make-clause :drop-column
                        :updated_on)
           (list "DROP COLUMN `updated_on`" nil))
    (is-mv (make-clause :rename-to :users)
           (list "RENAME TO `users`" nil))))

(deftest column-definition-clause-tests
  (testing "column definition with various attributes"
    (is-mv (sxql.clause::make-column-definition-clause
            (make-sql-symbol "name")
            :type (make-op :char (make-sql-variable 64))
            :not-null t
            :default (make-sql-variable "No Name"))
           '("`name` CHAR(64) NOT NULL DEFAULT ?" ("No Name")))

    (is-mv (sxql.clause::make-column-definition-clause
            (make-sql-symbol "id")
            :type (make-sql-keyword "BIGINT")
            :primary-key t
            :auto-increment t)
           '("`id` BIGINT AUTO_INCREMENT PRIMARY KEY" nil))

    (is-mv (sxql.clause::make-column-definition-clause
            (make-sql-symbol "email")
            :type (make-sql-keyword "TEXT")
            :not-null t
            :unique t)
           '("`email` TEXT NOT NULL UNIQUE" nil))))

(deftest conflict-resolution-clause-tests
  (testing "ON DUPLICATE KEY UPDATE clause"
    (is-mv (make-clause :on-duplicate-key-update :a 1 :b 2)
           '("ON DUPLICATE KEY UPDATE `a` = ?, `b` = ?" (1 2))))

  (testing "ON CONFLICT DO NOTHING clauses"
    (ok (equal (yield (make-clause :on-conflict-do-nothing))
               "ON CONFLICT DO NOTHING"))
    (ok (equal (yield (make-clause :on-conflict-do-nothing '(:x :y)))
               "ON CONFLICT (`x`, `y`) DO NOTHING"))
    (ok (equal (yield (make-clause :on-conflict-do-nothing :pkey))
               "ON CONFLICT ON CONSTRAINT `pkey` DO NOTHING"))))

(deftest on-conflict-do-update-clause-tests
  (testing "ON CONFLICT DO UPDATE with column sets"
    (is-mv (make-clause :on-conflict-do-update
                        '(:x :y)
                        (make-clause :set= :a 1 :b 2))
           '("ON CONFLICT (`x`, `y`) DO UPDATE SET `a` = ?, `b` = ?" (1 2)))

    (is-mv (make-clause :on-conflict-do-update
                        :pkey
                        (make-clause :set= :a 1 :b 2))
           '("ON CONFLICT ON CONSTRAINT `pkey` DO UPDATE SET `a` = ?, `b` = ?" (1 2))))

  (testing "ON CONFLICT DO UPDATE error conditions"
    (is-error (make-clause :on-conflict-do-update
                           nil
                           (make-clause :set= :a 1 :b 2))
              error))

  (testing "ON CONFLICT DO UPDATE with WHERE clause"
    (is-mv (make-clause :on-conflict-do-update
                        :pkey
                        (make-clause :set= :a 1 :b 2)
                        (make-clause :where (make-op := :x :y)))
           '("ON CONFLICT ON CONSTRAINT `pkey` DO UPDATE SET `a` = ?, `b` = ? WHERE (`x` = `y`)" (1 2))))

  (testing "ON CONFLICT DO UPDATE in INSERT statement"
    (is-mv (sxql.statement:make-statement :insert-into
                                          :table
                                          (make-clause :set= :a 1 :b 2)
                                          (make-clause :on-conflict-do-update
                                                       :pkey
                                                       (make-clause :set= :a 1 :b 2)))
           '("INSERT INTO `table` (`a`, `b`) VALUES (?, ?) ON CONFLICT ON CONSTRAINT `pkey` DO UPDATE SET `a` = ?, `b` = ?" (1 2 1 2)))))

(deftest sql-compile-clause-tests
  (testing "sql-compile with clauses"
    (ok (sql-compile (make-clause :limit 10)))
    (is-mv (sql-compile (make-clause :limit 10))
           '("LIMIT 10" ()))))

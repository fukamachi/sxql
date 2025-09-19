(defpackage #:sxql/test/sxql
  (:nicknames #:t.sxql)
  (:use #:cl
        #:sxql
        #:rove)
  (:import-from #:sxql/sql-type
                #:yield)
  (:shadowing-import-from #:sxql/test/prepare
                          #:is-error
                          #:is-mv))
(in-package #:sxql/test/sxql)

(setup
  (setf *quote-character* #\`))

(deftest select-statement-dsl-tests
  (testing "basic SELECT fields"
    (is-mv (select ((:+ 1 1)))
           '("SELECT (? + ?)" (1 1)))
    (is-mv (select :*)
           '("SELECT *" nil)))

  (testing "SELECT with WHERE clauses"
    (is-mv (select :* (from :table) (where (:+ 1 1)))
           '("SELECT * FROM `table` WHERE (? + ?)" (1 1)))
    (is-mv (select :* (from :table) (where (:is-null :name)))
           '("SELECT * FROM `table` WHERE (`name` IS NULL)" nil))
    (is-mv (select :* (from :table) (where (:or (:is-null :name)
                                                (:< :age 20))))
           '("SELECT * FROM `table` WHERE ((`name` IS NULL) OR (`age` < ?))" (20)))))

(deftest select-ordering-limiting-tests
  (testing "ORDER BY clauses"
    (is-mv (select :* (from :person) (order-by :age))
           '("SELECT * FROM `person` ORDER BY `age`" nil))
    (is-mv (select :* (from :person) (order-by (:desc :age)))
           '("SELECT * FROM `person` ORDER BY `age` DESC" nil))
    (is-mv (select :* (from :person) (order-by :age :id))
           '("SELECT * FROM `person` ORDER BY `age`, `id`" nil))
    (is-mv (select :* (from :person) (order-by (:desc :age) :id))
           '("SELECT * FROM `person` ORDER BY `age` DESC, `id`" nil)))

  (testing "LIMIT and OFFSET clauses"
    (is-mv (select :* (from :person) (order-by :age) (limit 5))
           '("SELECT * FROM `person` ORDER BY `age` LIMIT 5" nil))
    (is-mv (select :* (from :person) (order-by :age) (limit 0 5))
           '("SELECT * FROM `person` ORDER BY `age` LIMIT 0, 5" nil))
    (is-mv (select :* (from :person) (order-by :age) (limit 5) (offset 10))
           '("SELECT * FROM `person` ORDER BY `age` LIMIT 5 OFFSET 10" nil))))

(deftest select-aggregation-tests
  (testing "GROUP BY and aggregate functions"
    (is-mv (select ((:count :*)) (from :person) (group-by :country))
           '("SELECT COUNT(*) FROM `person` GROUP BY `country`" nil))
    (is-mv (select ((:raw "COUNT(*)")) (from :person) (group-by :country))
           '("SELECT (COUNT(*)) FROM `person` GROUP BY `country`" nil)))

  (testing "DISTINCT operations"
    (is-mv (select ((:distinct :age)) (from :person))
           '("SELECT DISTINCT `age` FROM `person`" nil))
    (is-mv (select (distinct-on (:name) :age) (from :person))
           '("SELECT DISTINCT ON (`name`) `age` FROM `person`" nil)))

  (testing "AS aliases in field list"
    (is-mv (select (:id (:as (:count :*) :count)) (from :person) (group-by :name))
           '("SELECT `id`, COUNT(*) AS `count` FROM `person` GROUP BY `name`" nil))))

(deftest select-join-tests
  (testing "LEFT JOIN with ON condition"
    (is-mv (select (:name :age)
                   (from (:as :person :p))
                   (left-join :config :on (:= :p.config_id :config.id))
                   (limit 5))
           '("SELECT `name`, `age` FROM `person` AS `p` LEFT JOIN `config` ON (`p`.`config_id` = `config`.`id`) LIMIT 5" nil)))

  (testing "LEFT JOIN with USING clause"
    (is-mv (select (:name :age)
                   (from (:as :person :p))
                   (left-join :config :using :config_id))
           '("SELECT `name`, `age` FROM `person` AS `p` LEFT JOIN `config` USING `config_id`" nil))
    (is-mv (select (:name :age)
                   (from (:as :person :p))
                   (left-join :config :using (:config_id :person_id)))
           '("SELECT `name`, `age` FROM `person` AS `p` LEFT JOIN `config` USING (`config_id`, `person_id`)" nil))))

(deftest select-subquery-tests
  (testing "subqueries in FROM clause"
    (is-mv (select :* (from (select :* (from :table)))
                   (where (:> :age 20)))
           '("SELECT * FROM (SELECT * FROM `table`) WHERE (`age` > ?)" (20)))
    (is-mv (select :* (from (:raw (yield (select :* (from :table)))))
                   (where '(:> :age 20)))
           '("SELECT * FROM (SELECT * FROM `table`) WHERE (`age` > ?)" (20)))))

(deftest select-sql-keywords-tests
  (testing "SQL keyword handling"
    (is-mv (select :* (from :table) (where (:= :a :null)))
           '("SELECT * FROM `table` WHERE (`a` = NULL)" ()))
    (is-mv (select :* (from :table) (where (:< :a :current_date)))
           '("SELECT * FROM `table` WHERE (`a` < CURRENT_DATE)" ()))
    (is-mv (select :* (from :table) (where (:< :a :current_time)))
           '("SELECT * FROM `table` WHERE (`a` < CURRENT_TIME)" ()))
    (is-mv (select :* (from :table) (where (:< :a :current_timestamp)))
           '("SELECT * FROM `table` WHERE (`a` < CURRENT_TIMESTAMP)" ()))))

(deftest dynamic-query-construction-tests
  (testing "dynamic values in queries"
    (let ((age-limit 20))
      (is-mv (select :* (from :table) (where (:< :age age-limit)))
             '("SELECT * FROM `table` WHERE (`age` < ?)" (20)))
      (is-mv (select ((:+ 1 age-limit)))
             '("SELECT (? + ?)" (1 20)))))

  (testing "dynamic field lists"
    (let ((field '(:id :name)))
      (is-mv (select field (from :table))
             '("SELECT `id`, `name` FROM `table`" ())))
    (let ((field '(:count :id)))
      (is-mv (select (list field) (from :table))
             '("SELECT COUNT(`id`) FROM `table`" ())))))

(deftest dynamic-table-column-tests
  (testing "dynamic table and alias construction"
    (let ((table :table)
          (table-alias "t"))
      (is-mv (select :* (from (:as :table (intern table-alias :keyword))))
             '("SELECT * FROM `table` AS `t`" ()))
      (is-mv (left-join (:as table (intern table-alias :keyword))
                        :on (:= (intern (format nil "~A.id" table-alias) :keyword) :table.id))
             '("LEFT JOIN `table` AS `t` ON (`t`.`id` = `table`.`id`)" ()))))

  (testing "dynamic column and limit construction"
    (let ((col :age))
      (is-mv (order-by col) '("ORDER BY `age`" ()))
      (is-mv (order-by (:desc col)) '("ORDER BY `age` DESC" ())))
    (let ((limit 10))
      (is-mv (limit limit) '("LIMIT 10" ()))
      (is-mv (limit 0 limit) '("LIMIT 0, 10" ()))
      (is-mv (offset limit) '("OFFSET 10" ())))))

(deftest insert-statement-dsl-tests
  (testing "basic INSERT INTO with SET"
    (is-mv (insert-into :person
                        (set= :name "Eitaro"
                              :sex "male"))
           '("INSERT INTO `person` (`name`, `sex`) VALUES (?, ?)" ("Eitaro" "male"))))

  (testing "INSERT INTO with SELECT"
    (is-mv (insert-into :person (:name :sex)
                        (select (:name :sex) (from :person_tmp)))
           '("INSERT INTO `person` (`name`, `sex`) SELECT `name`, `sex` FROM `person_tmp`" nil)))

  (testing "INSERT with ON DUPLICATE KEY UPDATE"
    (is-mv (insert-into :person
                        (set= :sex "male"
                              :age 25
                              :name "Eitaro Fukamachi")
                        (on-duplicate-key-update :age (:+ :age 1)))
           '("INSERT INTO `person` (`sex`, `age`, `name`) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE `age` = (`age` + ?)" ("male" 25 "Eitaro Fukamachi" 1)))))

(deftest update-statement-dsl-tests
  (testing "UPDATE with multiple clauses"
    (is-mv (update :person
                   (set= :name "Eitaro Fukamachi"
                         :sex "male")
                   (where (:> :age 20))
                   (order-by :id)
                   (limit 5))
           '("UPDATE `person` SET `name` = ?, `sex` = ? WHERE (`age` > ?) ORDER BY `id` LIMIT 5"
             ("Eitaro Fukamachi" "male" 20))))

  (testing "UPDATE with expression in SET"
    (is-mv (update :person
                   (set= :age (:+ :age 1))
                   (where (:like :name "Eitaro %")))
           '("UPDATE `person` SET `age` = (`age` + ?) WHERE (`name` LIKE ?)"
             (1 "Eitaro %")))))

(deftest delete-statement-dsl-tests
  (testing "DELETE FROM with JOIN and other clauses"
    (is-mv (delete-from :person
                        (left-join :config :on (:= :person.config_id :config.id))
                        (where (:< :age 20))
                        (order-by (:desc :age))
                        (limit 1))
           '("DELETE FROM `person` LEFT JOIN `config` ON (`person`.`config_id` = `config`.`id`) WHERE (`age` < ?) ORDER BY `age` DESC LIMIT 1" (20)))))

(deftest dynamic-crud-operations-tests
  (testing "dynamic CRUD operations with variables"
    (let ((table :person)
          (name "Eitaro"))
      (is-mv (set= :name name) '("SET `name` = ?" ("Eitaro")))
      (is-mv (set= :name (concatenate 'string name " Fukamachi")) '("SET `name` = ?" ("Eitaro Fukamachi")))
      (is-mv (insert-into table
                          (set= :name name))
             '("INSERT INTO `person` (`name`) VALUES (?)" ("Eitaro")))
      (is-mv (update table
                     (set= :name name))
             '("UPDATE `person` SET `name` = ?" ("Eitaro")))
      (is-mv (delete-from table (where (:= :name name)))
             '("DELETE FROM `person` WHERE (`name` = ?)" ("Eitaro"))))))

(deftest union-and-subquery-tests
  (testing "UNION operations"
    (is-mv (union-queries
            (select :* (from :table-1) (where (:= :a 10)))
            (select :* (from :table-2) (where (:> :b 100))))
           '("SELECT * FROM `table-1` WHERE (`a` = ?) UNION SELECT * FROM `table-2` WHERE (`b` > ?)"
             (10 100)))
    (is-mv (union-all-queries
            (select :* (from :table-1) (where (:= :a 10)))
            (select :* (from :table-2) (where (:> :b 100))))
           '("SELECT * FROM `table-1` WHERE (`a` = ?) UNION ALL SELECT * FROM `table-2` WHERE (`b` > ?)"
             (10 100))))

  (testing "subqueries in WHERE with quasiquote"
    (is-mv (where `(:<= ,(select :* (from :table)) :value))
           '("WHERE ((SELECT * FROM `table`) <= `value`)"
             nil))))

(deftest create-table-dsl-tests
  (testing "CREATE TABLE basic syntax"
    (is-mv (create-table :enemy
                         ((name :type 'string
                                :primary-key t)
                          (age :type 'integer
                               :not-null t)
                          (address :type 'text
                                   :not-null nil)
                          (fatal_weakness :type 'text
                                          :not-null t
                                          :default "None")
                          (identifying_color :type '(:char 20)
                                             :unique t)))
           '("CREATE TABLE `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None"))))

  (testing "CREATE TABLE with quoted table name"
    (is-mv (create-table (:enemy)
                         ((name :type 'string
                                :primary-key t)
                          (age :type 'integer
                               :not-null t)
                          (address :type 'text
                                   :not-null nil)
                          (fatal_weakness :type 'text
                                          :not-null t
                                          :default "None")
                          (identifying_color :type '(:char 20)
                                             :unique t)))
           '("CREATE TABLE `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None"))))

  (testing "CREATE TABLE IF NOT EXISTS"
    (is-mv (create-table (:enemy :if-not-exists t)
                         ((name :type 'string
                                :primary-key t)
                          (age :type 'integer
                               :not-null t)
                          (address :type 'text
                                   :not-null nil)
                          (fatal_weakness :type 'text
                                          :not-null t
                                          :default "None")
                          (identifying_color :type '(:char 20)
                                             :unique t)))
           '("CREATE TABLE IF NOT EXISTS `enemy` (
    `name` STRING PRIMARY KEY,
    `age` INTEGER NOT NULL,
    `address` TEXT,
    `fatal_weakness` TEXT NOT NULL DEFAULT ?,
    `identifying_color` CHAR(20) UNIQUE
)" ("None")))))

(deftest drop-table-dsl-tests
  (testing "DROP TABLE statements"
    (is-mv (drop-table :enemy)
           '("DROP TABLE `enemy`" nil))
    (is-mv (drop-table :enemy :if-exists t)
           '("DROP TABLE IF EXISTS `enemy`" nil)))

  (testing "DROP TABLE with dynamic table name"
    (let ((table-name :enemy))
      (is-mv (drop-table table-name :if-exists t)
             '("DROP TABLE IF EXISTS `enemy`" ())))))

(deftest index-dsl-tests
  (testing "CREATE INDEX statements"
    (is-mv (create-index :index_name
                         :unique t
                         :using :btree
                         :on '(:table :column1 :column2))
           '("CREATE UNIQUE INDEX `index_name` USING BTREE ON `table` (`column1`, `column2`)" nil))
    (is-mv (create-index :index_name
                         :unique t
                         :using :btree
                         :on '(:table :column1 :column2)
                         :if-not-exists t)
           '("CREATE UNIQUE INDEX `index_name` IF NOT EXISTS USING BTREE ON `table` (`column1`, `column2`)" nil)))

  (testing "DROP INDEX statements"
    (is-mv (drop-index "index_name" :if-exists t)
           '("DROP INDEX IF EXISTS `index_name`" nil))))

(deftest explain-dsl-tests
  (testing "EXPLAIN statements"
    (is-mv (explain (select :* (from :table)))
           '("EXPLAIN SELECT * FROM `table`" nil))
    (is-mv (explain (select :* (from :table)) :analyze t)
           '("EXPLAIN ANALYZE SELECT * FROM `table`" nil))
    (is-mv (explain (select :* (from :table)) :verbose t)
           '("EXPLAIN VERBOSE SELECT * FROM `table`" nil))))

(deftest placeholder-control-tests
  (testing "placeholder control with *use-placeholder*"
    (is-mv (let ((*use-placeholder* nil))
             (sql-compile (select :* (from :table) (where (:= :a "dog")))))
           '("SELECT * FROM `table` WHERE (`a` = 'dog')" ()))
    (is-mv (let ((*use-placeholder* nil))
             (sql-compile (select :* (from :table) (where (:= :a 101)))))
           '("SELECT * FROM `table` WHERE (`a` = 101)" ()))))

(deftest key-constraint-dsl-tests
  (testing "PRIMARY KEY constraints"
    (is-mv (primary-key :id)
           '("PRIMARY KEY (`id`)" nil))
    (is-mv (primary-key '(:id))
           '("PRIMARY KEY (`id`)" nil))
    (is-mv (primary-key "id_index" '(:id))
           '("PRIMARY KEY 'id_index' (`id`)" nil)))

  (testing "UNIQUE KEY constraints"
    (is-mv (unique-key "name_and_country_index" '(:name :country))
           '("UNIQUE 'name_and_country_index' (`name`, `country`)" nil)))

  (testing "FOREIGN KEY constraints"
    (is-mv (foreign-key :project_id :references '(:project :id))
           '("FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)" nil))
    (is-mv (foreign-key '(:project_id) :references '(:project :id))
           '("FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)" nil)))

  (testing "dynamic key constraints"
    (let ((primary-key :id))
      (is-mv (primary-key primary-key)
             '("PRIMARY KEY (`id`)" nil)))))

(deftest case-sensitivity-tests
  (testing "case sensitivity in table names"
    (is-mv (from :table)
           '("FROM `table`" nil))
    (is-mv (from :|Table|)
           '("FROM `Table`" nil))))
(in-package :cl-user)
(defpackage t.sxql
  (:use :cl
        :sxql
        :prove)
  (:import-from :sxql.sql-type
                :yield)
  (:shadowing-import-from :t.sxql.prepare
                          :is-error))
(in-package :t.sxql)

(plan 66)

(defmacro is-mv (test result &optional desc)
  `(is (multiple-value-list (yield ,test))
       ,result
       ,desc))

(is-mv (select ((:+ 1 1)))
       '("SELECT (? + ?)" (1 1))
       "field")
(is-mv (select :*)
       '("SELECT *" nil)
       "field")
(is-mv (select :* (from :table) (where (:+ 1 1)))
       '("SELECT * FROM `table` WHERE (? + ?)" (1 1))
       "where")
(is-mv (select :* (from :table) (where (:is-null :name)))
       '("SELECT * FROM `table` WHERE (`name` IS NULL)" nil)
       "where")
(is-mv (select :* (from :table) (where (:or (:is-null :name)
                                            (:< :age 20))))
       '("SELECT * FROM `table` WHERE ((`name` IS NULL) OR (`age` < ?))" (20))
       "where")
(is-mv (select :* (from :person) (order-by :age))
       '("SELECT * FROM `person` ORDER BY `age`" nil)
       "order by")
(is-mv (select :* (from :person) (order-by (:desc :age)))
       '("SELECT * FROM `person` ORDER BY `age` DESC" nil)
       "order by")
(is-mv (select :* (from :person) (order-by :age :id))
       '("SELECT * FROM `person` ORDER BY `age`, `id`" nil)
       "order by")
(is-mv (select :* (from :person) (order-by (:desc :age) :id))
       '("SELECT * FROM `person` ORDER BY `age` DESC, `id`" nil)
       "order by")
(is-mv (select :* (from :person) (order-by :age) (limit 5))
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 5" nil)
       "limit")
(is-mv (select :* (from :person) (order-by :age) (limit 0 5))
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 0, 5" nil)
       "limit")
(is-mv (select :* (from :person) (order-by :age) (limit 5) (offset 10))
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 5 OFFSET 10" nil)
       "offset")
(is-mv (select ((:count :*)) (from :person) (group-by :country))
       '("SELECT COUNT(*) FROM `person` GROUP BY `country`" nil)
       "group by")
(is-mv (select ((:raw "COUNT(*)")) (from :person) (group-by :country))
       '("SELECT (COUNT(*)) FROM `person` GROUP BY `country`" nil)
       "group by")
(is-mv (select ((:distinct :age)) (from :person))
       '("SELECT DISTINCT `age` FROM `person`" nil)
       "DISTINCT")
(is-mv (select (:id (:as (:count :*) :count)) (from :person) (group-by :name))
       '("SELECT `id`, COUNT(*) AS `count` FROM `person` GROUP BY `name`" nil)
       "AS in field list")

(is-mv (select (:name :age)
         (from (:as :person :p))
         (left-join :config :on (:= :p.config_id :config.id))
         (limit 5))
       '("SELECT `name`, `age` FROM `person` AS `p` LEFT JOIN `config` ON (`p`.`config_id` = `config`.`id`) LIMIT 5" nil)
       "LEFT JOIN")

(is-mv (select (:name :age)
         (from (:as :person :p))
         (left-join :config :using :config_id))
       '("SELECT `name`, `age` FROM `person` AS `p` LEFT JOIN `config` USING `config_id`" nil)
       "LEFT JOIN")

(is-mv (select (:name :age)
         (from (:as :person :p))
         (left-join :config :using (:config_id :person_id)))
       '("SELECT `name`, `age` FROM `person` AS `p` LEFT JOIN `config` USING (`config_id`, `person_id`)" nil)
       "LEFT JOIN")

(is-mv (select :* (from (select :* (from :table)))
               (where (:> :age 20)))
       '("SELECT * FROM (SELECT * FROM `table`) WHERE (`age` > ?)" (20))
       "subquery")
(is-mv (select :* (from (:raw (yield (select :* (from :table)))))
               (where '(:> :age 20)))
       '("SELECT * FROM (SELECT * FROM `table`) WHERE (`age` > ?)" (20))
       "subquery")

(is-mv (select :* (from :table) (where (:= :a :null)))
       '("SELECT * FROM `table` WHERE (`a` = NULL)" ())
       "NULL")

(let ((age-limit 20))
  (is-mv (select :* (from :table) (where (:< :age age-limit)))
         '("SELECT * FROM `table` WHERE (`age` < ?)" (20)))

  (is-mv (select ((:+ 1 age-limit)))
         '("SELECT (? + ?)" (1 20))))

(let ((field '(:id :name)))
  (is-mv (select field (from :table))
         '("SELECT `id`, `name` FROM `table`" ())))

(let ((field '(:count :id)))
  (is-mv (select (list field) (from :table))
         '("SELECT COUNT(`id`) FROM `table`" ())))

(let ((table :table)
      (table-alias "t"))
  (is-mv (select :* (from (:as :table (intern table-alias :keyword))))
         '("SELECT * FROM `table` AS `t`" ()))
  (is-mv (left-join (:as table (intern table-alias :keyword))
                    :on (:= (intern (format nil "~A.id" table-alias) :keyword) :table.id))
         '("LEFT JOIN `table` AS `t` ON (`t`.`id` = `table`.`id`)" ())))

(let ((col :age))
  (is-mv (order-by col) '("ORDER BY `age`" ()))
  (is-mv (order-by (:desc col)) '("ORDER BY `age` DESC" ())))

(let ((limit 10))
  (is-mv (limit limit) '("LIMIT 10" ()))
  (is-mv (limit 0 limit) '("LIMIT 0, 10" ()))
  (is-mv (offset limit) '("OFFSET 10" ())))

(is-mv (insert-into :person
                    (set= :name "Eitaro"
                          :sex "male"))
       '("INSERT INTO `person` (`name`, `sex`) VALUES (?, ?)" ("Eitaro" "male"))
       "INSERT INTO")

(is-mv (insert-into :person (:name :sex)
         (select (:name :sex) (from :person_tmp)))
       '("INSERT INTO `person` (`name`, `sex`) SELECT `name`, `sex` FROM `person_tmp`" nil)
       "INSERT INTO ... SELECT")

(is-mv (insert-into :person
         (set= :sex "male"
               :age 25
               :name "Eitaro Fukamachi")
         (on-duplicate-key-update :age (:+ :age 1)))
       '("INSERT INTO `person` (`sex`, `age`, `name`) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE `age` = (`age` + ?)" ("male" 25 "Eitaro Fukamachi" 1))
       "INSERT ... ON DUPLICATE KEY UPDATE")

(is-mv (update :person
               (set= :name "Eitaro Fukamachi"
                     :sex "male")
               (where (:> :age 20))
               (order-by :id)
               (limit 5))
       '("UPDATE `person` SET `name` = ?, `sex` = ? WHERE (`age` > ?) ORDER BY `id` LIMIT 5"
         ("Eitaro Fukamachi" "male" 20))
       "UPDATE")

(is-mv (update :person
         (set= :age (:+ :age 1))
         (where (:like :name "Eitaro %")))
       '("UPDATE `person` SET `age` = (`age` + ?) WHERE (`name` LIKE ?)"
         (1 "Eitaro %"))
       "UPDATE")

(is-mv (delete-from :person
                    (left-join :config :on (:= :person.config_id :config.id))
                    (where (:< :age 20))
                    (order-by (:desc :age))
                    (limit 1))
       '("DELETE FROM `person` LEFT JOIN `config` ON (`person`.`config_id` = `config`.`id`) WHERE (`age` < ?) ORDER BY `age` DESC LIMIT 1" (20))
       "DELETE FROM")

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
         '("DELETE FROM `person` WHERE (`name` = ?)" ("Eitaro"))))

(is-mv (union-queries
        (select :* (from :table-1) (where (:= :a 10)))
        (select :* (from :table-2) (where (:> :b 100))))
       '("(SELECT * FROM `table-1` WHERE (`a` = ?)) UNION (SELECT * FROM `table-2` WHERE (`b` > ?))"
         (10 100))
       "UNION")

(is-mv (union-all-queries
        (select :* (from :table-1) (where (:= :a 10)))
        (select :* (from :table-2) (where (:> :b 100))))
       '("(SELECT * FROM `table-1` WHERE (`a` = ?)) UNION ALL (SELECT * FROM `table-2` WHERE (`b` > ?))"
         (10 100))
       "UNION ALL")

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
)" ("None"))
       "CREATE TABLE")

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
)" ("None"))
       "CREATE TABLE")

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
)" ("None"))
       "CREATE TABLE IF NOT EXISTS")

(is-mv (drop-table :enemy)
       '("DROP TABLE `enemy`" nil)
       "DROP TABLE")

(is-mv (drop-table :enemy :if-exists t)
       '("DROP TABLE IF EXISTS `enemy`" nil)
       "DROP TABLE IF EXISTS")

(let ((table-name :enemy))
  (is-mv (drop-table table-name :if-exists t)
         '("DROP TABLE IF EXISTS `enemy`" ())))

(is-mv (create-index :index_name
                     :unique t
                     :using :btree
                     :on '(:table :column1 :column2))
       '("CREATE UNIQUE INDEX `index_name` USING BTREE ON `table` (`column1`, `column2`)" nil)
       "CREATE UNIQUE INDEX")

(is-mv (create-index :index_name
                     :unique t
                     :using :btree
                     :on '(:table :column1 :column2)
                     :if-not-exists t)
       '("CREATE UNIQUE INDEX `index_name` IF NOT EXISTS USING BTREE ON `table` (`column1`, `column2`)" nil)
       "CREATE UNIQUE INDEX IF NOT EXISTS")

(is-mv (drop-index "index_name" :if-exists t)
       '("DROP INDEX IF EXISTS `index_name`" nil)
       "DROP INDEX")

(diag "placeholder")

(is-mv (let ((*use-placeholder* nil))
         (sql-compile (select :* (from :table) (where (:= :a "dog")))))
       '("SELECT * FROM `table` WHERE (`a` = 'dog')" ())
       "Not use placeholder (string)")
(is-mv (let ((*use-placeholder* nil))
         (sql-compile (select :* (from :table) (where (:= :a 101)))))
       '("SELECT * FROM `table` WHERE (`a` = 101)" ())
       "Not use placeholder (integer)")

(diag "keys")

(is-mv (primary-key :id)
       '("PRIMARY KEY (`id`)" nil))
(is-mv (primary-key '(:id))
       '("PRIMARY KEY (`id`)" nil))
(is-mv (primary-key "id_index" '(:id))
       '("PRIMARY KEY 'id_index' (`id`)" nil))
(is-mv (unique-key "name_and_country_index" '(:name :country))
       '("UNIQUE 'name_and_country_index' (`name`, `country`)" nil))

(is-mv (foreign-key :project_id :references '(:project :id))
       '("FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)" nil))
(is-mv (foreign-key '(:project_id) :references '(:project :id))
       '("FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)" nil))

(let ((primary-key :id))
  (is-mv (primary-key primary-key)
         '("PRIMARY KEY (`id`)" nil)))

;; 20141231 - m@ahungry.com - Add tests to illustrate case sensitivity issue
(is-mv (from :table)
       '("FROM `table`" nil))

(is-mv (from :|Table|)
       '("FROM `Table`" nil))

;; 20141231 - m@ahungry.com - These above tests now pass, but all caps will fail
;; There likely isn't an easy work around, but I think all cap table names is rare
;; enough that it won't matter (maybe downcase can be something configured)
#+nil
(is-mv (from :|TABLE|)
       '("FROM `TABLE`" nil))

(finalize)

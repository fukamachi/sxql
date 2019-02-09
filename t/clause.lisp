(in-package :cl-user)
(defpackage t.sxql.clause
  (:use :cl
        :prove
        :sxql.sql-type
        :sxql.operator
        :sxql.clause
        :sxql.compile)
  (:shadowing-import-from :t.sxql.prepare
                          :is-error))
(in-package :t.sxql.clause)

(plan 59)

(ok (make-clause :where (make-op := :a 10)))
(is (multiple-value-list
     (yield (make-clause :where (make-op := :a 10))))
    (list "WHERE (`a` = ?)" '(10)))
(is-error (make-clause :where
                       (make-op := :a 10)
                       (make-op :!= :b 20))
          program-error)

(ok (make-clause :from (make-sql-symbol "table-name")))
(ok (make-clause :from (make-op :as :table-name :a)))
(is (multiple-value-list
     (yield (make-clause :from (make-sql-symbol "table-name"))))
    (list "FROM `table-name`" nil))
(is (multiple-value-list
     (yield (make-clause :from
                         (make-op :as :table-name :a))))
    (list "FROM `table-name` AS `a`" nil))

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

(is (multiple-value-list
     (yield
      (make-clause :having
                   (make-op :>= (make-sql-symbol "hoge") (make-sql-variable 88)))))
    (list "HAVING (`hoge` >= ?)" '(88)))

(is (multiple-value-list
     (yield
      (make-clause :returning (make-sql-symbol "id"))))
    (list "RETURNING `id`" nil))

(ok (make-clause :updatability :update) "FOR UPDATE")
(ok (make-clause :updatability :update :of '(:hoge :piyo)) "FOR UPDATE OF")
(ok (make-clause :updatability :update :of '(:hoge :piyo) :nowait t) "FOR UPDATE OF NOWAIT")
(is (multiple-value-list
     (yield (make-clause :updatability :update)))
    (list "FOR UPDATE" nil))
(is (multiple-value-list
     (yield (make-clause :updatability :share)))
    (list "FOR SHARE" nil))
(is (multiple-value-list
     (yield (make-clause :updatability :update :of '(:hoge :fuga.piyo))))
    (list "FOR UPDATE OF `hoge`, `fuga`.`piyo`" nil))
(is (multiple-value-list
     (yield (make-clause :updatability :update :of '(:hoge :piyo) :nowait t)))
    (list "FOR UPDATE OF `hoge`, `piyo` NOWAIT" nil))
(is (multiple-value-list
     (yield (make-clause :updatability :update :nowait t)))
    (list "FOR UPDATE NOWAIT" nil))

(ok (make-clause :limit (make-sql-variable 1)) "LIMIT")
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

(ok (make-clause :set= :a 1) "set=")
(ok (make-clause :set= :a 1 :b 2))
(is (multiple-value-list
     (yield (make-clause :set= :a 1 :b 2)))
    (list "SET `a` = ?, `b` = ?" '(1 2)))
(ok (make-clause :set= :a nil))
;(is-error (make-clause :set=) program-error)
;(is-error (make-clause :set= 'a 1 'b) program-error)
;(is-error (make-clause :set= '(a 1)) program-error)

(is (multiple-value-list
     (yield (make-clause :primary-key '(:id))))
    (list "PRIMARY KEY (`id`)" nil))
(is (multiple-value-list
     (yield (make-clause :primary-key :id)))
    (list "PRIMARY KEY (`id`)" nil))
(is (multiple-value-list
     (yield (make-clause :primary-key "primary_key_is_id"'(:id))))
    (list "PRIMARY KEY 'primary_key_is_id' (`id`)" nil))
(is (multiple-value-list
     (yield (make-clause :unique-key '(:name :country))))
    (list "UNIQUE (`name`, `country`)" nil))
(is (multiple-value-list
     (yield (make-clause :unique-key "name_and_country_index" '(:name :country))))
    (list "UNIQUE 'name_and_country_index' (`name`, `country`)" nil))
(is (multiple-value-list
     (yield (make-clause :key '(:id))))
    (list "KEY (`id`)" nil))
(is (multiple-value-list
     (yield (make-clause :key "id_is_unique" '(:id))))
    (list "KEY 'id_is_unique' (`id`)" nil))

(ok (sxql.clause::make-references-clause
     (sxql.sql-type:make-sql-symbol "project")
     (sxql.sql-type:make-sql-list (sxql.sql-type:make-sql-symbol "id"))))

(is (multiple-value-list
     (yield (make-clause :foreign-key '(:project_id) :references '(:project :id))))
    (list "FOREIGN KEY (`project_id`) REFERENCES `project` (`id`)" nil))

(is (yield (sxql.clause::make-sql-column-type-from-list '(:integer)))
    "INTEGER")
(is (yield (sxql.clause::make-sql-column-type-from-list '(:integer 11)))
    "INTEGER(11)")
(is (yield (sxql.clause::make-sql-column-type-from-list '(:integer 11 :unsigned)))
    "INTEGER(11) UNSIGNED")
(is (yield (sxql.clause::make-sql-column-type-from-list '(:integer nil :unsigned)))
    "INTEGER UNSIGNED")

(is (multiple-value-list
     (yield (make-clause :add-column :updated_at
                         :type 'integer
                         :default 0
                         :not-null t
                         :after :created_at)))
    (list "ADD COLUMN `updated_at` INTEGER NOT NULL DEFAULT ? AFTER `created_at`"
          '(0)))

(is (multiple-value-list
     (yield (make-clause :modify-column
                         :updated_at
                         :type 'datetime
                         :not-null t)))
    (list "MODIFY COLUMN `updated_at` DATETIME NOT NULL" nil))

(is (multiple-value-list
     (yield (make-clause :alter-column :user :type '(:varchar 64))))
    (list "ALTER COLUMN `user` TYPE VARCHAR(64)" nil))

(is (multiple-value-list
     (yield (make-clause :alter-column :id :set-default 1)))
    (list "ALTER COLUMN `id` SET DEFAULT ?" '(1)))

(is (multiple-value-list
     (yield (make-clause :alter-column :id :drop-default t)))
    (list "ALTER COLUMN `id` DROP DEFAULT" nil))

(is (multiple-value-list
     (yield (make-clause :alter-column :profile :not-null t)))
    (list "ALTER COLUMN `profile` SET NOT NULL" nil))

(is (multiple-value-list
     (yield (make-clause :drop-column
                         :updated_on)))
    (list "DROP COLUMN `updated_on`" nil))

(is (multiple-value-list
     (yield (make-clause :rename-to :users)))
    (list "RENAME TO `users`" nil))

(is (multiple-value-list
     (yield
      (sxql.clause::make-column-definition-clause
       (make-sql-symbol "name")
       :type (make-op :char (make-sql-variable 64))
       :not-null t
       :default (make-sql-variable "No Name"))))
    '("`name` CHAR(64) NOT NULL DEFAULT ?" ("No Name"))
    "column-definition")

(is (multiple-value-list
     (yield
      (sxql.clause::make-column-definition-clause
       (make-sql-symbol "id")
       :type (make-sql-keyword "BIGINT")
       :primary-key t
       :auto-increment t)))
    '("`id` BIGINT AUTO_INCREMENT PRIMARY KEY" nil)
    "column-definition")

(is (multiple-value-list
     (yield
      (sxql.clause::make-column-definition-clause
       (make-sql-symbol "email")
       :type (make-sql-keyword "TEXT")
       :not-null t
       :unique t)))
    '("`email` TEXT NOT NULL UNIQUE" nil)
    "column-definition")

(is (multiple-value-list
     (yield
      (make-clause :on-duplicate-key-update :a 1 :b 2)))
    '("ON DUPLICATE KEY UPDATE `a` = ?, `b` = ?" (1 2))
    "on-duplicate-key-update")

(is (yield (make-clause :on-conflict-do-nothing))
    "ON CONFLICT DO NOTHING"
    "on-conflitct-do-nothing no conflict target")

(is (yield (make-clause :on-conflict-do-nothing '(:x :y)))
    "ON CONFLICT (`x`, `y`) DO NOTHING"
    "on-conflitct-do-nothing column set")

(is (yield (make-clause :on-conflict-do-nothing :pkey))
    "ON CONFLICT ON CONSTRAINT `pkey` DO NOTHING"
    "on-conflitct-do-nothing index name")

(is (multiple-value-list
     (yield
      (make-clause :on-conflict-do-update
                   '(:x :y)
                   (make-clause :set= :a 1 :b 2))))
    '("ON CONFLICT (`x`, `y`) DO UPDATE SET `a` = ?, `b` = ?" (1 2))
    "on-conflict-do-update column set")

(is (multiple-value-list
     (yield
      (make-clause :on-conflict-do-update
                   :pkey
                   (make-clause :set= :a 1 :b 2))))
    '("ON CONFLICT ON CONSTRAINT `pkey` DO UPDATE SET `a` = ?, `b` = ?" (1 2))
    "on-conflict-do-update column set")

(is-error (make-clause :on-conflict-do-update
                       nil
                       (make-clause :set= :a 1 :b 2))
          error
          "on-conflict-do-update no conflict target is error")

(is (multiple-value-list
     (yield
      (make-clause :on-conflict-do-update
                   :pkey
                   (make-clause :set= :a 1 :b 2)
                   (make-clause :where (make-op := :x :y)))))
    '("ON CONFLICT ON CONSTRAINT `pkey` DO UPDATE SET `a` = ?, `b` = ? WHERE (`x` = `y`)" (1 2))
    "on-conflict-do-update with where")

(is (multiple-value-list
     (yield
      (sxql.statement:make-statement :insert-into
                                     :table
                                     (make-clause :set= :a 1 :b 2)
                                     (make-clause :on-conflict-do-update
                                                  :pkey
                                                  (make-clause :set= :a 1 :b 2)))))
    '("INSERT INTO `table` (`a`, `b`) VALUES (?, ?) ON CONFLICT ON CONSTRAINT `pkey` DO UPDATE SET `a` = ?, `b` = ?" (1 2 1 2))
    "on-conflict-do-update inside insert set= correct")

(diag "sql-compile clause")

(ok (sql-compile (make-clause :limit 10)))

(is (multiple-value-list
     (yield (sql-compile (make-clause :limit 10))))
    '("LIMIT 10" ()))

(finalize)

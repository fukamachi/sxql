#|
  This file is a part of sxql project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage t.sxql
  (:use :cl
        :sxql
        :cl-test-more)
  (:import-from :sxql.sql-type
                :yield))
(in-package :t.sxql)

(plan nil)

(defmacro is-mv (test result &optional desc)
  `(is (multiple-value-list (yield ,test))
       ,result
       ,desc))

(is-mv (select '(:+ 1 1))
       '("SELECT (? + ?)" (1 1))
       "field")
(is-mv (select :*)
       '("SELECT *" nil)
       "field")
(is-mv (select :* (from 'table) (where '(:+ 1 1)))
       '("SELECT * FROM `table` WHERE (? + ?)" (1 1))
       "where")
(is-mv (select :* (from 'table) (where '(:is-null name)))
       '("SELECT * FROM `table` WHERE (`name` IS NULL)" nil)
       "where")
(is-mv (select :* (from 'person) (order-by 'age))
       '("SELECT * FROM `person` ORDER BY `age`" nil)
       "order by")
(is-mv (select :* (from 'person) (order-by '(:desc age)))
       '("SELECT * FROM `person` ORDER BY `age` DESC" nil)
       "order by")
(is-mv (select :* (from 'person) (order-by 'age 'id))
       '("SELECT * FROM `person` ORDER BY `age`, `id`" nil)
       "order by")
(is-mv (select :* (from 'person) (order-by '(:desc age) 'id))
       '("SELECT * FROM `person` ORDER BY `age` DESC, `id`" nil)
       "order by")
(is-mv (select :* (from 'person) (order-by 'age) (limit 5))
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 5" nil)
       "limit")
(is-mv (select :* (from 'person) (order-by 'age) (limit 0 5))
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 0, 5" nil)
       "limit")
(is-mv (select :* (from 'person) (order-by 'age) (limit 5) (offset 10))
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 5 OFFSET 10" nil)
       "offset")
(is-mv (select '(:count :*) (from 'person) (group-by 'country))
       '("SELECT COUNT(*) FROM `person` GROUP BY `country`" nil)
       "group by")
(is-mv (select '(:raw "COUNT(*)") (from 'person) (group-by 'country))
       '("SELECT (COUNT(*)) FROM `person` GROUP BY `country`" nil)
       "group by")
(is-mv (select '(:distinct age) (from 'person))
       '("SELECT (DISTINCT `age`) FROM `person`" nil)
       "DISTINCT")

(is-mv (select '(name age)
               (from '(:as person p))
               (left-join 'config :on '(:= p.config_id config.id))
               (limit 5))
       '("SELECT (`name`, `age`) FROM (`person` AS `p`) LEFT JOIN `config` ON (`p`.`config_id` = `config`.`id`) LIMIT 5" nil)
       "LEFT JOIN")

(is-mv (select :* (from (select :* (from 'table)))
               (where '(:> age 20)))
       '("SELECT * FROM (SELECT * FROM `table`) WHERE (`age` > ?)" (20))
       "subquery")
(is-mv (select :* (from `(:raw ,(yield (select :* (from 'table)))))
               (where '(:> age 20)))
       '("SELECT * FROM (SELECT * FROM `table`) WHERE (`age` > ?)" (20))
       "subquery")

(is-mv (select :* (from 'table) (where '(:= a :null)))
       '("SELECT * FROM `table` WHERE (`a` = NULL)" ())
       "NULL")

(is-mv (insert-into 'person
                    (set= 'name "Eitarow"
                          'sex "male"))
       '("INSERT INTO `person` SET `name` = ?, `sex` = ?" ("Eitarow" "male"))
       "INSERT INTO")

(is-mv (update 'person
               (set= 'name "Eitarow Fukamachi"
                     'sex "male")
               (where '(:> age 20))
               (order-by 'id)
               (limit 5))
       '("UPDATE `person` SET `name` = ?, `sex` = ? WHERE (`age` > ?) ORDER BY `id` LIMIT 5"
         ("Eitarow Fukamachi" "male" 20))
       "UPDATE")

(is-mv (delete-from 'person
                    (left-join 'config :on '(:= person.config_id config.id))
                    (where '(:< age 20))
                    (order-by '(:desc age))
                    (limit 1))
       '("DELETE FROM `person` LEFT JOIN `config` ON (`person`.`config_id` = `config`.`id`) WHERE (`age` < ?) ORDER BY `age` DESC LIMIT 1" (20))
       "DELETE FROM")

(is-mv (union-queries
        (select '* (from 'table-1) (where '(:= a 10)))
        (select '* (from 'table-2) (where '(:> b 100))))
       '("(SELECT * FROM `table-1` WHERE (`a` = ?) UNION SELECT * FROM `table-2` WHERE (`b` > ?))"
         (10 100))
       "UNION")

(is-mv (union-all-queries
        (select '* (from 'table-1) (where '(:= a 10)))
        (select '* (from 'table-2) (where '(:> b 100))))
       '("(SELECT * FROM `table-1` WHERE (`a` = ?) UNION ALL SELECT * FROM `table-2` WHERE (`b` > ?))"
         (10 100))
       "UNION ALL")

(is-mv (create-table 'enemy
        '((name :type string
                :primary-key t)
          (age :type integer
               :not-null t)
          (address :type text
                   :not-null nil)
          (fatal_weakness :type text
                          :not-null t
                          :default "None")
          (identifying_color :type (:char 20)
                             :unique t)))
       '("CREATE TABLE `enemy` (`name` STRING PRIMARY KEY, `age` INTEGER NOT NULL, `address` TEXT, `fatal_weakness` TEXT NOT NULL DEFAULT ?, `identifying_color` CHAR(20) UNIQUE)" ("None"))
       "CREATE TABLE")

(is-mv (drop-table 'enemy)
       '("DROP TABLE `enemy`" nil)
       "DROP TABLE")

(is-mv (drop-table 'enemy :if-exists t)
       '("DROP TABLE IF EXISTS `enemy`" nil)
       "DROP TABLE IF EXISTS")

(finalize)

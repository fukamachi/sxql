#|
  This file is a part of sxql project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage t.sxql
  (:use :cl
        :sxql
        :cl-test-more))
(in-package :t.sxql)

(plan nil)

(defmacro is-mv (test result &optional desc)
  `(is (multiple-value-list ,test)
       ,result
       ,desc))

(is-mv (select :field '(:+ 1 1))
       '("SELECT (? + ?)" (1 1))
       "field")
(is-mv (select :field :*)
       '("SELECT *" nil)
       "field")
(is-mv (select :from 'table :where '(:+ 1 1))
       '("SELECT * FROM `table` WHERE (? + ?)" (1 1))
       "where")
(is-mv (select :from 'table :where '(:is-null name))
       '("SELECT * FROM `table` WHERE (`name` IS NULL)" nil)
       "where")
(is-mv (select :from 'person :order-by 'age)
       '("SELECT * FROM `person` ORDER BY `age`" nil)
       "order by")
(is-mv (select :from 'person :order-by '(:desc age))
       '("SELECT * FROM `person` ORDER BY `age` DESC" nil)
       "order by")
(is-mv (select :from 'person :order-by '(age id))
       '("SELECT * FROM `person` ORDER BY (`age`, `id`)" nil)
       "order by")
(is-mv (select :from 'person :order-by '((:desc age) id))
       '("SELECT * FROM `person` ORDER BY (`age` DESC, `id`)" nil)
       "order by")
(is-mv (select :from 'person :order-by 'age :limit 5)
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 5" nil)
       "limit")
(is-mv (select :from 'person :order-by 'age :limit '(0 5))
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 0, 5" nil)
       "limit")
(is-mv (select :from 'person :order-by 'age :limit 5 :offset 10)
       '("SELECT * FROM `person` ORDER BY `age` LIMIT 5 OFFSET 10" nil)
       "offset")
(is-mv (select :field '(:count :*) :from 'person :group-by 'country)
       '("SELECT COUNT(*) FROM `person` GROUP BY `country`" nil)
       "group by")
(is-mv (select :field '(:raw "COUNT(*)") :from 'person :group-by 'country)
       '("SELECT (COUNT(*)) FROM `person` GROUP BY `country`" nil)
       "group by")

(is-mv (select :from `(:raw ,(select :from 'table))
               :where '(:> age 20))
       '("SELECT * FROM (SELECT * FROM `table`) WHERE (`age` > ?)" (20))
       "subquery")

(finalize)

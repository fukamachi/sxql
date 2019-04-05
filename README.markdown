# SxQL - An SQL generator.

[![Build Status](https://travis-ci.org/fukamachi/sxql.svg?branch=master)](https://travis-ci.org/fukamachi/sxql)

## Usage

```common-lisp
(select (:id :name :sex)
  (from (:as :person :p))
  (where (:and (:>= :age 18)
               (:< :age 65)))
  (order-by (:desc :age)))
;=> #<SXQL-STATEMENT: SELECT id, name, sex FROM person AS p WHERE ((age >= 18) AND (age < 65)) ORDER BY age DESC>

(yield *)

;=> "SELECT id, name, sex FROM person AS p WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC"
;   (18 65)

(sql-compile **)
;=> #<SXQL-COMPILED: SELECT id, name, sex FROM person AS p WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC [18, 65]>

(union-queries * (select (:id :name :sex) (from '(:as animal a))))
;=> #<SXQL-OP: (SELECT id, name, sex FROM (person AS p) WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC) UNION (SELECT id, name, sex FROM (animal AS a))>

(yield *)
;=> "(SELECT id, name, sex FROM (person AS p) WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC) UNION (SELECT id, name, sex FROM (animal AS a))"
;   (18 65)
```

## SQL Statements

### select (field &body clauses)

Creates a SELECT query. It takes a field (or a list of fields) and SQL Clauses.

```common-lisp
(select ((:+ 1 1)))
;=> #<SXQL-STATEMENT: SELECT (1 + 1)>

(select :name
  (from :person)
  (where (:> :age 20)))
;=> #<SXQL-STATEMENT: SELECT name FROM person WHERE (age > 20)>

(select (:id :name)
  (from (:as :person :p))
  (left-join :person_config :on (:= :person.config_id :person_config.id))
  (where (:and (:> :age 20)
               (:<= :age 65)))
  (order-by :age)
  (limit 5))
;=> #<SXQL-STATEMENT: SELECT id, name FROM (person AS p) LEFT JOIN person_config ON (person.config_id = person_config.id) WHERE ((age > 20) AND (age <= 65)) ORDER BY age LIMIT 5>

(select (:sex (:count :*)) (from :person) (group-by :sex))
;=> #<SXQL-STATEMENT: SELECT sex, COUNT(*) FROM person GROUP BY sex>

(select (:sex (:as (:count :*) :num)) 
  (from :person)
  (group-by :sex)
  (order-by (:desc :num)))
;=> #<SXQL-STATEMENT: SELECT sex, COUNT(*) AS num FROM person GROUP BY sex ORDER BY num DESC>
```

### insert-into (table &body clauses)

```common-lisp
(insert-into :person
  (set= :sex "male"
        :age 25
        :name "Eitaro Fukamachi"))
;=> #<SXQL-STATEMENT: INSERT INTO person SET sex = 'male', age = 25, name = 'Eitaro Fukamachi'>

(insert-into :users
  (set= :name "Jack"
        :jinbei-size "small")
  (returning :id))
;=> #<SXQL-STATEMENT: INSERT INTO `users` (`name`, `jinbei-size`) VALUES ('Jack', 'small') RETURNING `id`>

(insert-into :person
  (:id :name)
  (select (:id :name)
    (from :person_tmp)))
;=> #<SXQL-STATEMENT: INSERT INTO person (id, name) SELECT id, name FROM person_tmp>
```

### update (table &body clauses)

```common-lisp
(update :person
  (set= :age 26)
  (where (:like :name "Eitaro %")))
;=> #<SXQL-STATEMENT: UPDATE person SET age = 26 WHERE (name LIKE 'Eitaro %')>
```

### delete-from (table &body clauses)

```common-lisp
(delete-from :person
  (where (:= :name "Eitaro Fukamachi")))
;=> #<SXQL-STATEMENT: DELETE FROM person WHERE (name = 'Eitaro Fukamachi')>
```

### union-queies (&rest statements)

```common-lisp
(union-queries
 (select (:name :birthday) (from :fulltime))
 (select (:name :birthday) (from :parttime)))
;=> #<SXQL-OP: (SELECT name, birthday FROM fulltime) UNION (SELECT name, birthday FROM parttime)>
```

### union-all-queries (&rest statements)

```common-lisp
(union-all-queries
 (select (:name :birthday) (from :fulltime))
 (select (:name :birthday) (from :parttime)))
;=> #<SXQL-OP: (SELECT name, birthday FROM fulltime) UNION ALL (SELECT name, birthday FROM parttime)>
```

### create-table (table column-definitions &body options)

```common-lisp
(create-table :enemy
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
;=> #<SXQL-STATEMENT: CREATE TABLE enemy (name STRING PRIMARY KEY, age INTEGER NOT NULL, address TEXT, fatal_weakness TEXT NOT NULL DEFAULT 'None', identifying_color CHAR(20) UNIQUE)>

(yield *)
;=> "CREATE TABLE enemy (name STRING PRIMARY KEY, age INTEGER NOT NULL, address TEXT, fatal_weakness TEXT NOT NULL DEFAULT ?, identifying_color CHAR(20) UNIQUE)"
;   ("None")

(create-table (:enemy :if-not-exists t)
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
;=> #<SXQL-STATEMENT: CREATE TABLE IF NOT EXISTS enemy (name STRING PRIMARY KEY, age INTEGER NOT NULL, address TEXT, fatal_weakness TEXT NOT NULL DEFAULT 'None', identifying_color CHAR(20) UNIQUE)>
```

### drop-table (table &key if-exists)

```common-lisp
(drop-table :enemy)
;=> #<SXQL-STATEMENT: DROP TABLE enemy>

(drop-table :enemy :if-exists t)
;=> #<SXQL-STATEMENT: DROP TABLE IF EXISTS enemy>
```

### alter-table (table &body clauses)

```common-lisp
(alter-table :tweet
  (add-column :id :type 'bigint :primary-key t :auto-increment t :first t)
  (add-column :updated_at :type 'timestamp))
;=> #<SXQL-STATEMENT: ALTER TABLE tweet ADD COLUMN id BIGINT AUTO_INCREMENT PRIMARY KEY FIRST, ADD COLUMN updated_at TIMESTAMP>
```

### create-index (index-name &key unique using on)

```common-lisp
(create-index "index_name"
              :unique t
              :using :btee
              :on '(:table :column1 :column2))
;=> #<SXQL-STATEMENT: CREATE UNIQUE INDEX index_name USING BTEE ON table (column1, column2)>
```

### drop-index (index-name &key if-exists on)

```common-lisp
(drop-index "index_name" :if-exists t :on :person)
;=> #<SXQL-STATEMENT: DROP INDEX IF EXISTS index_name ON person>
```

## SQL Clauses

### fields

```common-lisp
(fields :id)
;=> #<SXQL-CLAUSE: id>

(fields (:count :id))
;=> #<SXQL-CLAUSE: COUNT(id)>

(fields :id (:sum :amount))
;=> #<SXQL-CLAUSE: id, SUM(amount)>
```

### from

```common-lisp
(from :person)
;=> #<SXQL-CLAUSE: FROM person>

(from :person :person_config)
;=> #<SXQL-CLAUSE: FROM person, person_config>

(from (select :* (from :person) (where (:= :is_active 1))))
;=> #<SXQL-CLAUSE: FROM (SELECT * FROM person WHERE (is_active = 1))>
```

### where

```common-lisp
(where (:and (:> :age 20) (:<= :age 65)))
;=> #<SXQL-CLAUSE: WHERE ((age > 20) AND (age <= 65))>

(yield *)
;=> "WHERE ((age > ?) AND (age <= ?))"
;   (20 65)
```

### order-by

```common-lisp
(order-by :age)
;=> #<SXQL-CLAUSE: ORDER BY age>

(order-by :age (:desc :id))
;=> #<SXQL-CLAUSE: ORDER BY age, id DESC>
;   NIL
```

### group-by

```common-lisp
(group-by :sex)
;=> #<SXQL-CLAUSE: GROUP BY sex>
```

### having

```common-lisp
(having (:>= (:sum :hoge) 88))
;=> #<SXQL-CLAUSE: HAVING (SUM(`hoge`) >= 88)>
```

### returning

```common-lisp
(returning :id)
;=> #<SXQL-CLAUSE: RETURNING `id`>
```

### limit

```common-lisp
(limit 10)
;=> #<SXQL-CLAUSE: LIMIT 10>

(limit 0 10)
;=> #<SXQL-CLAUSE: LIMIT 0, 10>

(yield *)
;=> "LIMIT 0, 10"
;   NIL
```

### offset

```common-lisp
(offset 0)
;=> #<SXQL-CLAUSE: OFFSET 0>

(yield *)
;=> "OFFSET 0"
;   NIL
```

### inner-join, left-join, right-join, full-join

```common-lisp
(inner-join :person_config :on (:= :person.config_id :person_config.id))
;=> #<SXQL-CLAUSE: INNER JOIN person_config ON (person.config_id = person_config.id)>

(left-join :person_config :on (:= :person.config_id :person_config.id))
;=> #<SXQL-CLAUSE: LEFT JOIN person_config ON (person.config_id = person_config.id)>

(left-join :person_config :using :config_id)
;=> #<SXQL-CLAUSE: LEFT JOIN person_config USING config_id>
```

### primary-key

```common-lisp
(primary-key :id)
;=> #<SXQL-CLAUSE: PRIMARY KEY (id)>

(primary-key '(:id))
;=> #<SXQL-CLAUSE: PRIMARY KEY (id)>

(primary-key "id_index" '(:id))
;=> #<SXQL-CLAUSE: PRIMARY KEY 'id_index' (id)>
```

### unique-key

```common-lisp
(unique-key '(:name :country))
;=> #<SXQL-CLAUSE: UNIQUE (name, country)>

(unique-key "name_and_country_index" '(:name :country))
;=> #<SXQL-CLAUSE: UNIQUE 'name_and_country_index' (name, country)>
```

### index-key

```common-lisp
(index-key (:name :country))
;=> #<SXQL-CLAUSE: KEY (name, country)>

(index-key "name_and_country_index" '(:name :country))
;=> #<SXQL-CLAUSE: KEY 'name_and_country_index' (name, country)>
```

### foreign-key

```common-lisp
(foreign-key '(:project_id) :references '(:project :id))
;=> #<SXQL-CLAUSE: FOREIGN KEY (project_id) REFERENCES project (id)>

(foreign-key '(:user_id) :references '(:user :id) :on-delete :cascade)
;=> #<SXQL-CLAUSE: FOREIGN KEY (user_id) REFERENCES user (id) ON DELETE CASCADE>
```

### add-column

```common-lisp
(add-column :updated_at :type 'integer :default 0 :not-null t :after :created_at)
;=> #<SXQL-CLAUSE: ADD COLUMN updated_at INTEGER NOT NULL DEFAULT 0 AFTER created_at>
```

### modify-column

```common-lisp
(modify-column :updated_at :type 'datetime :not-null t)
;=> #<SXQL-CLAUSE: MODIFY COLUMN updated_at DATETIME NOT NULL>
```

### alter-column

```common-lisp
(alter-column :user :type '(:varchar 64))
;=> #<SXQL-CLAUSE: ALTER COLUMN user TYPE VARCHAR(64)>

(alter-column :id :set-default 1)
;=> #<SXQL-CLAUSE: ALTER COLUMN id SET DEFAULT 1>

(alter-column :id :drop-default t)
;=> #<SXQL-CLAUSE: ALTER COLUMN id DROP DEFAULT>

(alter-column :profile :not-null t)
;=> #<SXQL-CLAUSE: ALTER COLUMN profile SET NOT NULL>
```

### change-column

```common-lisp
(change-column :updated_at :updated_on)
;=> #<SXQL-CLAUSE: CHANGE COLUMN updated_at updated_on>
```

### drop-column

```common-lisp
(drop-column :updated_on)
;=> #<SXQL-CLAUSE: DROP COLUMN updated_on>
```

### add-primary-key

```common-lisp
(add-primary-key :id :name)
;=> #<SXQL-CLAUSE: ADD PRIMARY KEY (id, name)>
```

### drop-primary-key

```common-lisp
(drop-primary-key)
;=> #<SXQL-CLAUSE: DROP PRIMARY KEY>
```

### rename-to

```common-lisp
(rename-to :users)
;=> #<SXQL-CLAUSE: RENAME TO `users`>

(alter-table :user
  (rename-to :users))
;=> #<SXQL-STATEMENT: ALTER TABLE `user` RENAME TO `users`>
```

### on-duplicate-key-update

Support MySQL's `INSERT ... ON DUPLICATE KEY UPDATE` syntax.

```common-lisp
(on-duplicate-key-update :age (:+ :age 1))
;=> #<SXQL-CLAUSE: ON DUPLICATE KEY UPDATE `age` = (`age` + 1)>

(insert-into :person
  (set= :sex "male"
        :age 25
        :name "Eitaro Fukamachi")
  (on-duplicate-key-update :age (:+ :age 1)))
;=> #<SXQL-STATEMENT: INSERT INTO `person` (`sex`, `age`, `name`) VALUES ('male', 25, 'Eitaro Fukamachi') ON DUPLICATE KEY UPDATE `age` = (`age` + 1)>
```

### on-coflict-do-nothing

Support PostgreSQL's `INSERT ... ON CONFLICT DO NOTHING` syntax.

```common-lisp
(on-conflict-do-nothing)
;=> #<SXQL-CLAUSE: ON CONFLICT DO NOTHING>

(on-conflict-do-nothing :index_name)
;=> #<SXQL-CLAUSE: ON CONFLICT ON CONSTRAINT index_name DO NOTHING>

(on-conflict-do-nothing '(:column1 :column2 :column3))
;=> #<SXQL-CLAUSE: ON CONFLICT (column1, column2, column3) DO NOTHING>
```

### on-coflict-do-update

Support PostgreSQL's `INSERT ... ON CONFLICT ... DO UPDATE` syntax.

```common-lisp
(on-conflict-do-update :index_name (set= :x 1 :y 2))
;=> #<SXQL-CLAUSE: ON CONFLICT ON CONSTRAINT index_name DO UPDATE SET x = 1, y = 2>

(on-conflict-do-update '(:column1 :column2 :column3) (set= :x 1 :y 2))
;=> #<SXQL-CLAUSE: ON CONFLICT (column1, column2, column3) DO UPDATE SET x = 1, y = 2>

(insert-into :person
  (set= :sex "male"
        :age 25
        :name "Eitaro Fukamachi")
  (on-conflict-do-update '(:name)
                         (set= :age (:+ :age 1))
                         (where (:< :age 99))))
;=> #<SXQL-STATEMENT: INSERT INTO person (sex, age, name) VALUES ('male', 25, 'Eitaro Fukamachi') ON CONFLICT (name) DO UPDATE SET age = (age + 1) WHERE (age < 99)>
```

## SQL Operators

* :not
* :is-null, :not-null
* :asc, :desc
* :distinct
* :=, :!=
* :<, :>, :<= :>=
* :a<, :a>
* :as
* :in, :not-in
* :like
* :and, :or
* :+, :-, :* :/ :%
* :raw
* :is-distinct-from, :is-not-distinct-from (Postgres)

## Set a quote character

`*quote-character*` is the character that a table or column name will be quoted with. The default value is NIL (not quote).

```common-lisp
(yield (select :* (from 'table)))
;=> "SELECT * FROM table"
;   NIL

;; for MySQL
(let ((*quote-character* #\`))
  (yield (select :* (from 'table))))
;=> "SELECT * FROM `table`"
;   NIL

;; for PostgreSQL
(let ((*quote-character* #\"))
  (yield (select :* (from 'table))))
;=> "SELECT * FROM "table""
;   NIL
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2013-2014 Eitaro Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 3-Clause License.

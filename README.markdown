# SxQL - A SQL generator.

## Usage

```common-lisp
(select '(id name sex)
  (from '(:as person p))
  (where '(:and (:>= age 18)
                (:< age 65)))
  (order-by '(:desc age)))
;=> #<SXQL-STATEMENT: SELECT (id, name, sex) FROM (person AS p) WHERE ((age >= 18) AND (age < 65)) ORDER BY age DESC>

(yield *)
;=> "SELECT (id, name, sex) FROM (person AS p) WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC"
;   (18 65)

(sql-compile **)
;=> #<SXQL-COMPILED: SELECT (id, name, sex) FROM (person AS p) WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC [18, 65]>

(union-queries * (select '(id name sex) (from '(:as animal a))))
;=> #<SXQL-OP: (SELECT (id, name, sex) FROM (person AS p) WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC UNION SELECT (id, name, sex) FROM (animal AS a))>

(yield *)
;=> "(SELECT (id, name, sex) FROM (person AS p) WHERE ((age >= ?) AND (age < ?)) ORDER BY age DESC UNION SELECT (id, name, sex) FROM (animal AS a))"
;   (18 65)
```

## SQL Statements

### select (field &rest clauses)

Creates a SELECT query. It takes a field (or a list of fields) and SQL Clauses.

```common-lisp
(select '(:+ 1 1))
;=> #<SXQL-STATEMENT: SELECT (1 + 1)>

(select 'name
  (from 'person)
  (where '(:> age 20)))
;=> #<SXQL-STATEMENT: SELECT name FROM person WHERE (age > 20)>

(select '(id name)
  (from '(:as person p))
  (left-join 'person_config :on '(:= person.config_id person_config.id))
  (where '(:and (:> age 20)
                (:<= age 65)))
  (order-by 'age)
  (limit 5))
;=> #<SXQL-STATEMENT: SELECT (id, name) FROM (person AS p) LEFT JOIN person_config ON (person.config_id = person_config.id) WHERE ((age > 20) AND (age <= 65)) ORDER BY age LIMIT 5>

(select '(sex (:count :*)) (from 'person) (group-by 'sex))
;=> #<SXQL-STATEMENT: SELECT (sex, COUNT(*)) FROM person GROUP BY sex>
```

### insert-into (table &rest clauses)

```common-lisp
(insert-into 'person
  (set= 'sex "male"
        'age 25
        'name "Eitarow Fukamachi"))
;=> #<SXQL-STATEMENT: INSERT INTO person SET sex = 'male', age = 25, name = 'Eitarow Fukamachi'>
```

### update (table &rest clauses)

```common-lisp
(update 'person
  (set= 'age 26)
  (where '(:like name "Eitarow %")))
;=> #<SXQL-STATEMENT: UPDATE person SET age = 26 WHERE (name LIKE 'Eitarow %')>
```

### delete-from (table &rest clauses)

```common-lisp
(delete-from 'person
  (where '(:= name "Eitarow Fukamachi")))
;=> #<SXQL-STATEMENT: DELETE FROM person WHERE (name = 'Eitarow Fukamachi')>
```

### union-queies (&rest statements)

```common-lisp
(union-queries
 (select '(name birthday) (from 'fulltime))
 (select '(name birthday) (from 'parttime)))
;=> #<SXQL-OP: (SELECT (name, birthday) FROM fulltime UNION SELECT (name, birthday) FROM parttime)>
```

### union-all-queries (&rest statements)

```common-lisp
(union-all-queries
 (select '(name birthday) (from 'fulltime))
 (select '(name birthday) (from 'parttime)))
;=> #<SXQL-OP: (SELECT (name, birthday) FROM fulltime UNION ALL SELECT (name, birthday) FROM parttime)>
```

### create-table

```common-lisp
(create-table 'enemy
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
;=> #<SXQL-STATEMENT: CREATE TABLE enemy (name STRING PRIMARY KEY, age INTEGER NOT NULL, address TEXT, fatal_weakness TEXT NOT NULL DEFAULT 'None', identifying_color CHAR(20) UNIQUE)>

(yield *)
;=> "CREATE TABLE enemy (name STRING PRIMARY KEY, age INTEGER NOT NULL, address TEXT, fatal_weakness TEXT NOT NULL DEFAULT ?, identifying_color CHAR(20) UNIQUE)"
;   ("None")
```

### drop-table

```common-lisp
(drop-table 'enemy)
;=> #<SXQL-STATEMENT: DROP TABLE enemy>

(drop-table 'enemy :if-exists t)
;=> #<SXQL-STATEMENT: DROP TABLE IF EXISTS enemy>
```

## SQL Clauses

### from

```common-lisp
(from 'person)
;=> #<SXQL-CLAUSE: FROM person>
```

### where

```common-lisp
(where '(:and (:> age 20) (:<= age 65)))
;=> #<SXQL-CLAUSE: WHERE ((age > 20) AND (age <= 65))>

(yield *)
;=> "WHERE ((age > ?) AND (age <= ?))"
;   (20 65)
```

### order-by

```common-lisp
(order-by 'age)
;=> #<SXQL-CLAUSE: ORDER BY age>

(order-by 'age '(:desc id))
;=> #<SXQL-CLAUSE: ORDER BY age, id DESC>
;   NIL
```

### group-by

```common-lisp
(group-by 'sex)
;=> #<SXQL-CLAUSE: GROUP BY sex>
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

### left-join

```common-lisp
(left-join 'person_config :on '(:= person.config_id person_config.id))
;=> #<SXQL-CLAUSE: LEFT JOIN person_config ON (person.config_id = person_config.id)>
```

## SQL Operators

* :not
* :is-null, :not-null
* :asc, :desc
* :distinct
* :=, :!=
* :<, :>, :<= :>=
* :as
* :in, :not-in
* :like
* :and, :or
* :+, :-, :* :/ :%
* :raw

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

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 3-Clause License.

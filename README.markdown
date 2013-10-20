# SxQL - A SQL generator.

## Usage

```common-lisp
(select '(id name sex)
  (from '(:as person p))
  (where '(:and (:>= age 18)
                (:< age 65)))
  (order-by '(:desc age)))
;=> #<SXQL-STATEMENT: SELECT (`id`, `name`, `sex`) FROM (`person` AS `p`) WHERE ((`age` >= 18) AND (`age` < 65)) ORDER BY `age` DESC>

(yield *)
;=> "SELECT (`id`, `name`, `sex`) FROM (`person` AS `p`) WHERE ((`age` >= ?) AND (`age` < ?)) ORDER BY `age` DESC"
;   (18 65)
```

## SQL Statements

### select

## SQL Clauses

### from

### where

### order-by

### group-by

### limit

### left-join

## SQL Operators

### not

### is-null, not-null

### asc, desc

### distinct

### =, !=

### <, >, <= >=

### as

### in, not-in

### like

### and, or

### +, -, * / %

### raw

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)

# License

Licensed under the BSD 3-Clause License.

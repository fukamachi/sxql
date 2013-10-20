(in-package :cl-user)
(defpackage t.sxql.sql-type
  (:use :cl
        :sxql
        :sxql.sql-type
        :cl-test-more))
(in-package :t.sxql.sql-type)

(plan nil)

(ok (make-sql-variable 1))
(ok (make-sql-variable "Hello"))
(is-error (make-sql-variable 'hello) type-error)
(is-error (make-sql-variable :hello) type-error)
(is-error (make-sql-variable nil) type-error)
(is-error (make-sql-variable '(hello)) type-error)
(ok (make-sql-expression-list (make-sql-variable 1)))
(is-error (make-sql-expression-list 1) type-error)

(is (multiple-value-list (yield (make-sql-variable 1)))
    (list "?" '(1)))

(is (multiple-value-list (yield (make-sql-variable "a")))
    (list "?" '("a")))

(ok (make-sql-keyword "NULL"))
(is-error (make-sql-keyword 1) type-error)
(is-error (make-sql-keyword 'null) type-error)
(is-error (make-sql-keyword :null) type-error)
(is-error (make-sql-keyword nil) type-error)
(is-error (make-sql-keyword '(null)) type-error)

(is (multiple-value-list (yield (make-sql-keyword "NULL")))
    (list "NULL" nil))

(ok (make-sql-symbol "column-name"))
(ok (make-sql-symbol "table.column-name"))
(is-error (make-sql-symbol 1) type-error)
(is-error (make-sql-symbol 'null) type-error)
(is-error (make-sql-symbol :null) type-error)
(is-error (make-sql-symbol nil) type-error)
(is-error (make-sql-symbol '(null)) type-error)

(is (multiple-value-list (yield (make-sql-symbol "column-name")))
    (list "`column-name`" nil))
(is (multiple-value-list (yield (make-sql-symbol "table.column-name")))
    (list "`table`.`column-name`" nil))

(ok (make-sql-atom-list (make-sql-variable 1)
                        (make-sql-variable 2)
                        (make-sql-variable 3)))
(ok (make-sql-atom-list (make-sql-variable 1)
                        (make-sql-symbol "a")
                        (make-sql-keyword "NULL")))
(ok (make-sql-atom-list))
(is-error (make-sql-atom-list 1) type-error)
(is-error (make-sql-atom-list nil) type-error)
(is-error (make-sql-atom-list (make-sql-variable 1)
                              "a")
          type-error)

(is (multiple-value-list
     (yield (make-sql-atom-list (make-sql-variable 1)
                                (make-sql-variable 2)
                                (make-sql-variable "3"))))
    (list "(?, ?, ?)" '(1 2 "3")))
(is (multiple-value-list
     (yield (make-sql-atom-list (make-sql-variable 1)
                                (make-sql-variable 2)
                                (make-sql-keyword "NULL")
                                (make-sql-symbol "a")
                                (make-sql-symbol "table.b"))))
    (list "(?, ?, NULL, `a`, `table`.`b`)" '(1 2)))

(finalize)

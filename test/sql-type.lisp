(defpackage #:sxql/test/sql-type
  (:nicknames #:t.sxql.sql-type)
  (:use #:cl
        #:sxql
        #:sxql/sql-type
        #:rove)
  (:shadowing-import-from #:sxql/test/prepare
                          #:is-error
                          #:is-mv))
(in-package #:sxql/test/sql-type)

(setup
  (setf *quote-character* #\`))

(teardown
  (setf *quote-character* nil))

(deftest sql-variable-tests
  (testing "sql-variable creation with valid inputs"
    (ok (make-sql-variable 1))
    (ok (make-sql-variable "Hello")))

  (testing "sql-variable creation with invalid inputs"
    (is-error (make-sql-variable 'hello) type-error)
    (is-error (make-sql-variable :hello) type-error)
    (is-error (make-sql-variable nil) type-error)
    (is-error (make-sql-variable '(hello)) type-error))

  (testing "sql-variable yield generates correct SQL"
    (is-mv (make-sql-variable 1) (list "?" '(1)))
    (is-mv (make-sql-variable "a") (list "?" '("a")))))

(deftest sql-expression-list-tests
  (testing "sql-expression-list creation"
    (ok (make-sql-expression-list (make-sql-variable 1)))
    (is-error (make-sql-expression-list 1) type-error)))

(deftest sql-keyword-tests
  (testing "sql-keyword creation with valid inputs"
    (ok (make-sql-keyword "NULL")))

  (testing "sql-keyword creation with invalid inputs"
    (is-error (make-sql-keyword 1) type-error)
    (is-error (make-sql-keyword 'null) type-error)
    (is-error (make-sql-keyword :null) type-error)
    (is-error (make-sql-keyword nil) type-error)
    (is-error (make-sql-keyword '(null)) type-error))

  (testing "sql-keyword yield generates correct SQL"
    (is-mv (make-sql-keyword "NULL") (list "NULL" nil))))

(deftest sql-symbol-tests
  (testing "sql-symbol creation with valid inputs"
    (ok (make-sql-symbol "column-name"))
    (ok (make-sql-symbol "table.column-name"))
    (ok (make-sql-symbol "table.*")))

  (testing "sql-symbol creation with invalid inputs"
    (is-error (make-sql-symbol 1) type-error)
    (is-error (make-sql-symbol 'null) type-error)
    (is-error (make-sql-symbol :null) type-error)
    (is-error (make-sql-symbol nil) type-error)
    (is-error (make-sql-symbol '(null)) type-error))

  (testing "sql-symbol yield generates correct SQL with quoting"
    (is-mv (make-sql-symbol "column-name") (list "`column-name`" nil))
    (is-mv (make-sql-symbol "table.column-name") (list "`table`.`column-name`" nil))
    (is-mv (make-sql-symbol "table.*") (list "`table`.*" nil))))

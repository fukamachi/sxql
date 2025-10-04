(defpackage #:sxql/test/composer-multi
  (:use #:cl
        #:rove)
  (:import-from #:sxql/composer
                #:->
                #:insert-query-state-p
                #:update-query-state-p
                #:delete-query-state-p)
  (:import-from #:sxql
                #:insert-into
                #:update
                #:delete-from
                #:where
                #:order-by
                #:limit
                #:set=
                #:returning
                #:yield))
(in-package #:sxql/test/composer-multi)

(defun assert-sql-equal (expected-sql expected-params query)
  (multiple-value-bind (sql params) (yield query)
    (ok (equal sql expected-sql))
    (ok (equal params expected-params))))

(deftest v2-insert-basic-tests
  (testing "Basic INSERT statement composition"

    (testing "Simple INSERT with SET="
      (let ((q (-> (insert-into :users)
                   (set= :name "john" :email "john@example.com"))))
        (ok (insert-query-state-p q))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "INSERT INTO" sql))
          (ok (search "VALUES" sql))
          (ok (equal params '("john" "john@example.com"))))))

    (testing "INSERT with RETURNING"
      (let ((q (-> (insert-into :users)
                   (set= :name "jane" :email "jane@example.com")
                   (returning :id))))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "RETURNING" sql))
          (ok (equal params '("jane" "jane@example.com"))))))))

(deftest v2-update-basic-tests
  (testing "Basic UPDATE statement composition"

    (testing "Simple UPDATE with SET and WHERE"
      (let ((q (-> (update :users)
                   (set= :name "updated")
                   (where (:= :id 123)))))
        (ok (update-query-state-p q))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "UPDATE" sql))
          (ok (search "SET" sql))
          (ok (search "WHERE" sql))
          (ok (equal params '("updated" 123))))))

    (testing "UPDATE with multiple WHERE clauses"
      (let ((q (-> (update :users)
                   (set= :status "active")
                   (where (:= :id 1))
                   (where (:= :verified 1)))))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "AND" sql))
          (ok (equal params '("active" 1 1))))))

    (testing "UPDATE with ORDER BY and LIMIT"
      (let ((q (-> (update :users)
                   (set= :score 100)
                   (where (:< :score 50))
                   (order-by :created_at)
                   (limit 10))))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "ORDER BY" sql))
          (ok (search "LIMIT" sql))
          (ok (equal params '(100 50))))))))

(deftest v2-delete-basic-tests
  (testing "Basic DELETE statement composition"

    (testing "Simple DELETE with WHERE"
      (let ((q (-> (delete-from :users)
                   (where (:= :id 123)))))
        (ok (delete-query-state-p q))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "DELETE FROM" sql))
          (ok (search "WHERE" sql))
          (ok (equal params '(123))))))

    (testing "DELETE with multiple WHERE clauses"
      (let ((q (-> (delete-from :posts)
                   (where (:= :user_id 1))
                   (where (:< :created_at "2020-01-01")))))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "AND" sql))
          (ok (equal params '(1 "2020-01-01"))))))

    (testing "DELETE with ORDER BY and LIMIT"
      (let ((q (-> (delete-from :logs)
                   (where (:< :created_at "2020-01-01"))
                   (order-by (:asc :created_at))
                   (limit 1000))))
        (multiple-value-bind (sql params) (yield q)
          (ok (search "ORDER BY" sql))
          (ok (search "LIMIT" sql))
          (ok (equal params '("2020-01-01"))))))))

(deftest v2-update-immutability-tests
  (testing "UPDATE query-state immutability"

    (testing "Base UPDATE query remains unchanged"
      (let ((base (-> (update :users)
                      (set= :status "active"))))
        (let ((with-where (-> base (where (:= :id 1))))
              (with-limit (-> base (limit 10))))
          ;; Verify each derivation works independently
          (let ((sql1 (yield with-where))
                (sql2 (yield with-limit)))
            (ok (search "WHERE" sql1))
            (ok (not (search "WHERE" sql2)))
            (ok (not (search "LIMIT" sql1)))
            (ok (search "LIMIT" sql2))))))))

(deftest v2-delete-immutability-tests
  (testing "DELETE query-state immutability"

    (testing "Base DELETE query remains unchanged"
      (let ((base (-> (delete-from :logs)
                      (where (:< :created_at "2020-01-01")))))
        (let ((with-limit (-> base (limit 100)))
              (with-order (-> base (order-by :created_at))))
          ;; Verify each derivation works independently
          (let ((sql1 (yield with-limit))
                (sql2 (yield with-order)))
            (ok (search "LIMIT" sql1))
            (ok (not (search "LIMIT" sql2)))
            (ok (not (search "ORDER BY" sql1)))
            (ok (search "ORDER BY" sql2))))))))

(deftest v2-statement-conversion-tests
  (testing "v1 to v2 statement conversion"

    (testing "INSERT statement conversion"
      (let* ((v1-stmt (insert-into :users (:name) (values "test")))
             (q (-> v1-stmt)))
        (ok (insert-query-state-p q))
        (let ((sql (yield q)))
          (ok (search "INSERT INTO" sql)))))

    (testing "UPDATE statement conversion"
      (let* ((v1-stmt (update :users (set= :name "test") (where (:= :id 1))))
             (q (-> v1-stmt)))
        (ok (update-query-state-p q))
        (let ((sql (yield q)))
          (ok (search "UPDATE" sql)))))

    (testing "DELETE statement conversion"
      (let* ((v1-stmt (delete-from :users (where (:= :id 1))))
             (q (-> v1-stmt)))
        (ok (delete-query-state-p q))
        (let ((sql (yield q)))
          (ok (search "DELETE FROM" sql)))))))

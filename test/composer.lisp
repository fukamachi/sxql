(defpackage #:sxql/test/composer
  (:use #:cl
        #:rove)
  (:import-from #:sxql/composer
                #:->
                #:register-table-columns
                #:clear-column-mappings
                #:select-query-state-p
                #:select-query-state-where-clauses
                #:select-query-state-order-by-clauses
                #:select-query-state-group-by-clauses
                #:select-query-state-having-clauses
                #:select-query-state-join-clauses
                #:select-query-state-limit-clause
                #:select-query-state-offset-clause)
  (:import-from #:sxql
                #:select
                #:from
                #:where
                #:order-by
                #:group-by
                #:having
                #:limit
                #:offset
                #:fields
                #:returning
                #:join
                #:inner-join
                #:left-join
                #:right-join
                #:yield))
(in-package #:sxql/test/composer)

;;
;; Test Helpers
;;

(defun assert-sql-equal (expected-sql expected-params query)
  "Helper to test SQL generation with consistent assertion pattern"
  (multiple-value-bind (sql params) (yield query)
    (ok (equal sql expected-sql))
    (ok (equal params expected-params))))

(defmacro testing-sql (description expected-sql expected-params query-form)
  "Macro to reduce repetitive SQL testing patterns"
  `(testing ,description
     (assert-sql-equal ,expected-sql ,expected-params ,query-form)))

(deftest v2-basic-query-generation-tests
  (testing "Basic query building and SQL generation"

    (testing-sql "Simple query with table and basic WHERE clause"
                 "SELECT * FROM users WHERE (id = ?)"
                 '(123)
                 (-> (from :users)
                     (where (:= :id 123))))

    (testing "Query with ORDER BY clause"
      (let* ((query (-> (from :posts)
                        (order-by (:desc :created_at)))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal sql "SELECT * FROM posts ORDER BY created_at DESC"))
          (ok (null params)))))

    (testing "Query with both WHERE and ORDER BY"
      (let* ((query (-> (from :blog)
                        (where (:= :user_id 456))
                        (order-by (:asc :title)))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal sql "SELECT * FROM blog WHERE (user_id = ?) ORDER BY title ASC"))
          (ok (equal params '(456))))))))

(deftest v2-threading-macro-tests
  (testing "Threading macro behavior and query state management"

    (testing "Building query incrementally"
      (let* ((step1 (from :users))
             (step2 (-> step1 (where (:= :active 1))))
             (step3 (-> step2 (order-by (:desc :created_at)))))
        (ok (select-query-state-p step2))
        (ok (select-query-state-p step3))
        ;; Step 2 should have where clauses but not order-by
        (ok (= 1 (length (select-query-state-where-clauses step2))))
        (ok (null (select-query-state-order-by-clauses step2)))
        ;; Step 3 should have both
        (ok (= 1 (length (select-query-state-where-clauses step3))))
        (ok (= 1 (length (select-query-state-order-by-clauses step3))))))

    (testing "Immutability of query states"
      (let* ((original (from :users))
             (with-where (-> original (where (:= :id 123)))))
        ;; New query should have the where clause
        (ok (= 1 (length (select-query-state-where-clauses with-where))))))))

(deftest v2-clause-composition-tests
  (testing "Multiple clause handling and composition"

    (testing "Multiple WHERE clauses"
      (let* ((query (-> (from :posts)
                        (where (:= :user_id 123))
                        (where (:= :status "published")))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal sql "SELECT * FROM posts WHERE ((user_id = ?) AND (status = ?))"))
          (ok (equal params '(123 "published"))))))

    (testing "Multiple ORDER BY clauses"
      (let* ((query (-> (from :posts)
                        (order-by (:desc :created_at))
                        (order-by (:asc :title)))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal sql "SELECT * FROM posts ORDER BY created_at DESC, title ASC"))
          (ok (null params)))))

    (testing "Complex query with multiple clause types"
      (let* ((query (-> (from :orders)
                        (where (:= :customer_id 789))
                        (where (:> :total 100))
                        (order-by (:desc :created_at))
                        (order-by (:asc :id)))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal sql "SELECT * FROM orders WHERE ((customer_id = ?) AND (total > ?)) ORDER BY created_at DESC, id ASC"))
          (ok (equal params '(789 100))))))))

(deftest v2-sql-generation-tests
  (testing "SQL string generation and parameter handling"

    (testing "Parameterized queries"
      (let* ((query (-> (from :users)
                        (where (:= :email "test@example.com"))
                        (where (:>= :age 21)))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal params '("test@example.com" 21)))
          (ok (equal sql "SELECT * FROM users WHERE ((email = ?) AND (age >= ?))")))))

    (testing "Quote character handling"
      (let* ((sxql:*quote-character* #\`)
             (query (-> (from :user_profiles) (fields :*))))
        (multiple-value-bind (sql params) (yield query)
          ;; Table names should be quoted
          (ok (equal sql "SELECT * FROM `user_profiles`"))
          (ok (null params)))))

    (testing "SQL keyword case"
      (let* ((query (-> (from :products)
                        (where (:= :category "electronics")))))
        (multiple-value-bind (sql params) (yield query)
          ;; SQL keywords should be uppercase
          (ok (equal sql "SELECT * FROM products WHERE (category = ?)"))
          (ok (equal params '("electronics"))))))))

(deftest v2-edge-cases-tests
  (testing "Edge cases and error conditions"

    (testing "Special characters in values"
      (let* ((query (-> (from :comments)
                        (where (:= :content "It's a \"quoted\" string")))))
        (multiple-value-bind (sql params) (yield query)
          (ok (string= "It's a \"quoted\" string" (first params)))
          ;; SQL should use placeholders, not literal strings
          (ok (search "?" sql)))))

    (testing "NULL values"
      (let* ((query (-> (from :users)
                        (where (:= :deleted_at nil)))))
        (multiple-value-bind (sql params) (yield query)
          ;; NULL values in SxQL are rendered as () and don't generate parameters
          (ok (= 0 (length params)))
          (ok (search "()" sql))))))

    (testing "Boolean values"
      (let* ((query (-> (from :features)
                        (where (:= :enabled 1)))))
        (multiple-value-bind (sql params) (yield query)
          (declare (ignore sql))
          (ok (= 1 (first params)))
          (ok (= 1 (length params)))))))

(deftest v2-complex-clause-combinations-tests
  (testing "Complex queries with multiple clause types"

    (testing "Complete query with all major clauses"
      (let* ((query (-> (select (:users.name :posts.title) (from :users))
                        (inner-join :posts :on (:= :users.id :posts.author_id))
                        (where (:= :users.active 1))
                        (group-by :users.id)
                        (having (:> (:count :posts.id) 5))
                        (order-by (:desc :users.name))
                        (limit 20)
                        (offset 10))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal sql "SELECT posts.title, users.name FROM users INNER JOIN posts ON (users.id = posts.author_id) WHERE (users.active = ?) GROUP BY users.id HAVING (COUNT(posts.id) > ?) ORDER BY users.name DESC LIMIT 20 OFFSET 10"))
          (ok (equal params '(1 5))))))

    (testing "Threading macro with multiple clauses"
      (let* ((query (-> (from :products)
                        (fields :name :price :category)
                        (where (:and (:>= :price 100) (:= :active 1)))
                        (order-by (:desc :price) :name)
                        (limit 25))))
        (multiple-value-bind (sql params) (yield query)
          (ok (equal sql "SELECT category, price, name FROM products WHERE ((price >= ?) AND (active = ?)) ORDER BY price DESC, name LIMIT 25"))
          (ok (equal params '(100 1))))))))

(deftest v2-immutability-tests
  (testing "Query state immutability and reusable base queries"

    (testing "Base query remains unchanged after derivations"
      ;; Create base query using -> so we get a query-state
      (let ((base-query (-> (select (:*) (from :users))
                            (where (:= :is_active 1)))))
        ;; Record initial state
        (let ((initial-where-count (length (select-query-state-where-clauses base-query)))
              (initial-order-count (length (select-query-state-order-by-clauses base-query))))

          ;; Create multiple derivations
          (let ((recent-users (-> base-query (where (:< "2025-01-01" :created_at))))
                (search-users (-> base-query (where (:like :name "%foo%"))))
                (ordered-users (-> base-query (order-by :created_at))))

            ;; Verify base query unchanged
            (ok (= initial-where-count (length (select-query-state-where-clauses base-query))))
            (ok (= initial-order-count (length (select-query-state-order-by-clauses base-query))))

            ;; Verify derivations work correctly
            (ok (= 2 (length (select-query-state-where-clauses recent-users))))
            (ok (= 2 (length (select-query-state-where-clauses search-users))))
            (ok (= 1 (length (select-query-state-order-by-clauses ordered-users))))

            ;; Verify they generate different SQL
            (let ((recent-sql (yield recent-users))
                  (search-sql (yield search-users)))
              (ok (search "created_at" recent-sql))
              (ok (search "LIKE" search-sql))
              (ok (not (equal recent-sql search-sql))))))))

    (testing "All clause types maintain immutability"
      ;; Create base query as query-state using ->
      (let ((base (-> (select (:id) (from :users)))))
        (let ((with-where (-> base (where (:= :id 1))))
              (with-order (-> base (order-by :name)))
              (with-group (-> base (group-by :status)))
              (with-having (-> base (having (:> :count 5))))
              (with-join (-> base (inner-join :posts :on (:= :users.id :posts.user_id))))
              (with-limit (-> base (limit 10)))
              (with-offset (-> base (offset 20))))

          ;; Base query should remain empty for all clause types
          (ok (= 0 (length (select-query-state-where-clauses base))))
          (ok (= 0 (length (select-query-state-order-by-clauses base))))
          (ok (= 0 (length (select-query-state-group-by-clauses base))))
          (ok (= 0 (length (select-query-state-having-clauses base))))
          (ok (= 0 (length (select-query-state-join-clauses base))))
          (ok (null (select-query-state-limit-clause base)))
          (ok (null (select-query-state-offset-clause base)))

          ;; Each derivation should have exactly one clause of its type
          (ok (= 1 (length (select-query-state-where-clauses with-where))))
          (ok (= 1 (length (select-query-state-order-by-clauses with-order))))
          (ok (= 1 (length (select-query-state-group-by-clauses with-group))))
          (ok (= 1 (length (select-query-state-having-clauses with-having))))
          (ok (= 1 (length (select-query-state-join-clauses with-join))))
          (ok (not (null (select-query-state-limit-clause with-limit))))
          (ok (not (null (select-query-state-offset-clause with-offset)))))))

    (testing "Real-world usage patterns work correctly"
      ;; This tests the exact pattern mentioned in the user's requirements
      ;; Wrap with -> to ensure we get a query-state
      (let ((active-users (-> (select (:*)
                                      (from :users)
                                      (where (:= :is_active 1))))))

        ;; Multiple independent usages of the base query
        (let ((recent-users (-> active-users (where (:< "2025-01-01" :created_at))))
              (search-users (-> active-users (where (:like :name "%foo%")))))

          ;; Base query should be reusable
          (ok (= 1 (length (select-query-state-where-clauses active-users))))

          ;; Each usage should work independently
          (ok (= 2 (length (select-query-state-where-clauses recent-users))))
          (ok (= 2 (length (select-query-state-where-clauses search-users))))

          ;; SQL generation should work for all
          (let ((base-sql (yield active-users))
                (recent-sql (yield recent-users))
                (search-sql (yield search-users)))
            (ok (search "is_active" base-sql))
            (ok (search "is_active" recent-sql))
            (ok (search "created_at" recent-sql))
            (ok (search "is_active" search-sql))
            (ok (search "LIKE" search-sql))))))))

(deftest v2-auto-qualification-tests
  (testing "Automatic column qualification when JOINs are added"

    (testing "Basic auto-qualification with single JOIN"
      ;; Create base query with unqualified columns
      (let ((base-query (-> (select (:*)
                                    (from :users)
                                    (where (:= :is_active 1))))))

        ;; Add JOIN - should automatically qualify existing columns
        (let ((joined-query (-> base-query
                                (inner-join :blogs :on (:= :blogs.user_id :users.id)))))

          ;; Generate SQL for both queries
          (let ((base-sql (yield base-query))
                (joined-sql (yield joined-query)))

            ;; Base query should have unqualified column
            (ok (equal base-sql "SELECT * FROM users WHERE (is_active = ?)"))
            ;; Joined query should have qualified column
            (ok (equal joined-sql "SELECT * FROM users INNER JOIN blogs ON (blogs.user_id = users.id) WHERE (users.is_active = ?)"))))))

    (testing "Multiple WHERE clauses with auto-qualification"
      (let ((base-query (select (fields :*)
                                (from :users)
                                (where (:= :is_active 1))
                                (where (:> :age 18)))))

        (let ((joined-query (-> base-query
                                (left-join :posts :on (:= :posts.author_id :users.id)))))

          (let ((joined-sql (yield joined-query)))
            ;; Both WHERE conditions should be qualified
            (ok (search "users.is_active" joined-sql))
            (ok (search "users.age" joined-sql))
            (ok (search "LEFT JOIN" joined-sql))))))

    (testing "ORDER BY clauses with auto-qualification"
      (let ((base-query (select (fields :*)
                                (from :users)
                                (where (:= :is_active 1))
                                (order-by :created_at :name))))

        (let ((joined-query (-> base-query
                                (right-join :profiles :on (:= :profiles.user_id :users.id)))))

          (let ((joined-sql (yield joined-query)))
            ;; WHERE and ORDER BY should be qualified
            (ok (search "users.is_active" joined-sql))
            (ok (search "users.created_at" joined-sql))
            (ok (search "users.name" joined-sql))
            (ok (search "RIGHT JOIN" joined-sql))))))

    (testing "HAVING clauses with auto-qualification"
      (let ((base-query (select (fields :*)
                                (from :users)
                                (where (:= :is_active 1))
                                (group-by :department)
                                (having (:> :salary 50000)))))

        (let ((joined-query (-> base-query
                                (inner-join :departments :on (:= :departments.id :users.dept_id)))))

          (let ((joined-sql (yield joined-query)))
            ;; All clauses should be qualified
            (ok (equal joined-sql "SELECT * FROM users INNER JOIN departments ON (departments.id = users.dept_id) WHERE (users.is_active = ?) GROUP BY department HAVING (users.salary > ?)"))))))

    (testing "Pre-qualified columns remain unchanged"
      (let ((base-query (select (fields :*)
                                (from :users)
                                (where (:= :users.is_active 1))  ; Already qualified
                                (where (:= :status "pending"))))) ; Unqualified

        (let ((joined-query (-> base-query
                                (inner-join :orders :on (:= :orders.user_id :users.id)))))

          (let ((joined-sql (yield joined-query)))
            ;; Pre-qualified should stay qualified, unqualified should be qualified
            (ok (equal joined-sql "SELECT * FROM users INNER JOIN orders ON (orders.user_id = users.id) WHERE ((users.is_active = ?) AND (users.status = ?))"))))))

    (testing "Multiple JOINs - qualification only happens on first JOIN"
      (let ((base-query (select (fields :*)
                                (from :users)
                                (where (:= :is_active 1)))))

        ;; First JOIN triggers qualification
        (let ((first-join (-> base-query
                              (inner-join :posts :on (:= :posts.author_id :users.id)))))

          ;; Second JOIN should not re-qualify
          (let ((second-join (-> first-join
                                 (left-join :comments :on (:= :comments.post_id :posts.id)))))

            (let ((final-sql (yield second-join)))
              ;; Should have qualified columns and both JOINs
              (ok (equal final-sql
                         "SELECT * FROM users INNER JOIN posts ON (posts.author_id = users.id) LEFT JOIN comments ON (comments.post_id = posts.id) WHERE (users.is_active = ?)")))))))

    (testing "Real-world usage pattern matching user requirements"
      ;; This tests the exact scenario described in the requirements
      (let ((active-users (select (fields :*)
                                  (from :users)
                                  (where (:= :is_active 1)))))

        ;; Add JOIN with additional WHERE - should auto-qualify existing columns
        (let ((users-with-blogs (-> active-users
                                    (inner-join :blogs :on (:= :blogs.user_id :users.id))
                                    (where (:like :blogs.name "%My Blog%")))))

          (let ((result-sql (yield users-with-blogs)))
            (ok (equal result-sql
                       "SELECT * FROM users INNER JOIN blogs ON (blogs.user_id = users.id) WHERE ((users.is_active = ?) AND (blogs.name LIKE ?))"))))))

    (testing "Complex expressions with auto-qualification"
      (let ((base-query (select (fields :*)
                                (from :users)
                                (where (:and (:= :is_active 1)
                                             (:or (:> :age 21) (:< :score 100)))))))

        (let ((joined-query (-> base-query
                                (inner-join :memberships :on (:= :memberships.user_id :users.id)))))

          (let ((joined-sql (yield joined-query)))
            (ok (equal joined-sql
                       "SELECT * FROM users INNER JOIN memberships ON (memberships.user_id = users.id) WHERE ((users.is_active = ?) AND ((users.age > ?) OR (users.score < ?)))"))))))))

(deftest composer-conditional-composition-tests
  (testing "Conditional composition with NIL forms"

    (testing "when forms returning NIL are skipped"
      (let ((include-filter nil)
            (include-order t))
        (let ((q (-> (from :users)
                     (when include-filter (where (:= :active 1)))
                     (when include-order (order-by :name)))))
          (multiple-value-bind (sql params) (yield q)
            ;; Should have ORDER BY but no WHERE
            (ok (search "ORDER BY" sql))
            (ok (not (search "WHERE" sql)))
            (ok (null params))))))

    (testing "Multiple conditional clauses"
      (let ((filter-active t)
            (filter-role nil)
            (sort-by-name t)
            (include-limit nil))
        (let ((q (-> (from :users)
                     (when filter-active (where (:= :active 1)))
                     (when filter-role (where (:= :role "admin")))
                     (when sort-by-name (order-by :name))
                     (when include-limit (limit 10)))))
          (multiple-value-bind (sql params) (yield q)
            ;; Should have WHERE for active and ORDER BY, but no role filter or LIMIT
            (ok (search "WHERE" sql))
            (ok (search "active" sql))
            (ok (not (search "role" sql)))
            (ok (search "ORDER BY" sql))
            (ok (not (search "LIMIT" sql)))
            (ok (equal params '(1)))))))

    (testing "Dynamic query building with conditionals"
      (defun build-query (&key active role min-age)
        (-> (from :users)
            (when active (where (:= :active 1)))
            (when role (where (:= :role role)))
            (when min-age (where (:>= :age min-age)))
            (order-by :name)))

      (testing "All conditions present"
        (let ((q (build-query :active t :role "admin" :min-age 18)))
          (multiple-value-bind (sql params) (yield q)
            (ok (search "active" sql))
            (ok (search "role" sql))
            (ok (search "age" sql))
            (ok (equal params '(1 "admin" 18))))))

      (testing "Some conditions NIL"
        (let ((q (build-query :active t :min-age 21)))
          (multiple-value-bind (sql params) (yield q)
            (ok (search "active" sql))
            (ok (not (search "role" sql)))
            (ok (search "age" sql))
            (ok (equal params '(1 21))))))

      (testing "All conditions NIL"
        (let ((q (build-query)))
          (multiple-value-bind (sql params) (yield q)
            (ok (not (search "WHERE" sql)))
            (ok (search "ORDER BY" sql))
            (ok (null params))))))

    (testing "Mixing regular and conditional clauses"
      (let ((include-extra-filter nil))
        (let ((q (-> (from :users)
                     (where (:= :active 1))  ; Always included
                     (when include-extra-filter (where (:= :verified 1)))
                     (order-by :created_at))))  ; Always included
          (multiple-value-bind (sql params) (yield q)
            (ok (search "WHERE" sql))
            (ok (search "active" sql))
            (ok (not (search "verified" sql)))
            (ok (search "ORDER BY" sql))
            (ok (equal params '(1)))))))))

(deftest composer-table-alias-tests
  (testing "FROM clause table aliases (GitHub issue #92)"

    (testing "Simple table alias in composed query"
      (let ((q (-> (select :*)
                   (from (:as :a_very_long_name :a))
                   (where (:= :a.id 1)))))
        (multiple-value-bind (sql params) (yield q)
          (ok (equal sql "SELECT * FROM a_very_long_name AS a WHERE (a.id = ?)"))
          (ok (equal params '(1))))))

    (testing "Table alias with multiple clauses"
      (let ((q (-> (select :*)
                   (from (:as :users :u))
                   (where (:= :u.active 1))
                   (order-by :u.name))))
        (multiple-value-bind (sql params) (yield q)
          (ok (equal sql "SELECT * FROM users AS u WHERE (u.active = ?) ORDER BY u.name"))
          (ok (equal params '(1))))))

    (testing "Table alias with JOIN"
      (let ((q (-> (select :*)
                   (from (:as :users :u))
                   (left-join (:as :posts :p) :on (:= :u.id :p.author_id))
                   (where (:= :u.active 1)))))
        (multiple-value-bind (sql params) (yield q)
          (ok (equal sql "SELECT * FROM users AS u LEFT JOIN posts AS p ON (u.id = p.author_id) WHERE (u.active = ?)"))
          (ok (equal params '(1))))))

    (testing "Starting from select statement with alias"
      (let ((q (-> (select :* (from (:as :products :p)))
                   (where (:> :p.price 100)))))
        (multiple-value-bind (sql params) (yield q)
          (ok (equal sql "SELECT * FROM products AS p WHERE (p.price > ?)"))
          (ok (equal params '(100))))))))

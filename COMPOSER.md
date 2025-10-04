# SxQL Query Composer

## Overview

SxQL Query Composer introduces an immutable, composable query builder that allows you to construct SQL queries incrementally using a threading macro. This approach provides better ergonomics for building complex queries dynamically while maintaining immutability.

## Key Features

- **Immutable query composition**: Each operation returns a new query state
- **Thread-first macro (`->`)**: Clean, readable query construction
- **Multi-statement support**: Works with SELECT, INSERT, UPDATE, and DELETE
- **Backward compatible**: Seamlessly interoperates with v1 API
- **Type-safe**: Separate query-state types for each statement kind

## Quick Start

```common-lisp
(use-package :sxql)

;; Simple SELECT query
(-> (select :*)
    (from :users)
    (where (:= :active 1))
    (order-by :name)
    (limit 10))

;; UPDATE query
(-> (update :users)
    (set= :status "active")
    (where (:= :id 123)))

;; DELETE query
(-> (delete-from :logs)
    (where (:< :created_at "2020-01-01"))
    (limit 1000))

;; INSERT query
(-> (insert-into :users)
    (set= :name "Alice" :email "alice@example.com")
    (returning :id))
```

## The `->` Threading Macro

The `->` (thread-first) macro is the primary interface for building queries in composer. It takes an initial value and threads it through a series of transformations.

### Basic Usage

```common-lisp
;; Start with a statement
(-> (select :id :name)
    (from :users)
    (where (:= :active 1)))

;; Or start with just a clause
(-> (from :users)
    (where (:= :active 1))
    (order-by :created_at))
;; When starting with a clause, SELECT is assumed
```

### How It Works

1. The first argument is converted to a query-state
2. Each subsequent form is a clause that gets added to the query-state
3. A single copy is made at the beginning, then clauses are added destructively for efficiency
4. The result is a new query-state that can be yielded to SQL

```common-lisp
;; These are equivalent:
(-> (select :*) (from :users) (where (:= :id 1)))

;; Expands roughly to:
(let ((q (copy-query-state (select-statement-to-query-state (select :*)))))
  (add-clause q (from :users))
  (add-clause q (where (:= :id 1)))
  q)
```

## Immutability and Reusability

One of the key benefits of composer is immutable query composition. You can build a base query and derive multiple variants without affecting the original.

```common-lisp
;; Create a base query
(defvar *base-query*
  (-> (from :users)
      (where (:= :active 1))))

;; Derive different variants
(defvar *admin-users*
  (-> *base-query*
      (where (:= :role "admin"))
      (order-by :name)))

(defvar *recent-users*
  (-> *base-query*
      (where (:> :created_at "2024-01-01"))
      (order-by (:desc :created_at))
      (limit 50)))

;; *base-query* remains unchanged
(yield *base-query*)
;=> "SELECT * FROM users WHERE (active = ?)" (1)

(yield *admin-users*)
;=> "SELECT * FROM users WHERE ((active = ?) AND (role = ?)) ORDER BY name" (1 "admin")

(yield *recent-users*)
;=> "SELECT * FROM users WHERE ((active = ?) AND (created_at > ?)) ORDER BY created_at DESC LIMIT 50" (1 "2024-01-01")
```

## Query State Types

Composer introduces specific types for each kind of SQL statement:

### `select-query-state`

Container for SELECT queries.

**Slots:**
- `fields` - List of fields to select
- `where-clauses` - List of WHERE conditions
- `order-by-clauses` - List of ORDER BY clauses
- `group-by-clauses` - List of GROUP BY expressions
- `having-clauses` - List of HAVING conditions
- `join-clauses` - List of JOIN clauses
- `limit-clause` - LIMIT clause
- `offset-clause` - OFFSET clause
- `primary-table` - Main table name (from base type)
- `returning-clause` - RETURNING clause (from base type)

**Example:**
```common-lisp
(-> (select :id :name :email)
    (from :users)
    (where (:= :active 1))
    (order-by :created_at)
    (limit 10))
```

### `insert-query-state`

Container for INSERT queries.

**Slots:**
- `columns` - List of column names
- `values-list` - List of VALUES clauses
- `set-clause` - SET= clause for simple inserts
- `select-subquery` - SELECT subquery for INSERT ... SELECT
- `on-duplicate-key-clause` - ON DUPLICATE KEY UPDATE clause
- `on-conflict-clause` - ON CONFLICT clause
- `primary-table` - Table name (from base type)
- `returning-clause` - RETURNING clause (from base type)

**Example:**
```common-lisp
(-> (insert-into :users)
    (set= :name "Alice" :email "alice@example.com")
    (returning :id))
```

### `update-query-state`

Container for UPDATE queries.

**Slots:**
- `set-clause` - SET= clause
- `where-clauses` - List of WHERE conditions
- `order-by-clauses` - List of ORDER BY clauses
- `join-clauses` - List of JOIN clauses
- `limit-clause` - LIMIT clause
- `primary-table` - Table name (from base type)
- `returning-clause` - RETURNING clause (from base type)

**Example:**
```common-lisp
(-> (update :users)
    (set= :status "inactive")
    (where (:= :id 123))
    (returning :*))
```

### `delete-query-state`

Container for DELETE queries.

**Slots:**
- `where-clauses` - List of WHERE conditions
- `order-by-clauses` - List of ORDER BY clauses
- `join-clauses` - List of JOIN clauses
- `limit-clause` - LIMIT clause
- `primary-table` - Table name (from base type)
- `returning-clause` - RETURNING clause (from base type)

**Example:**
```common-lisp
(-> (delete-from :logs)
    (where (:< :created_at "2020-01-01"))
    (order-by :created_at)
    (limit 10000))
```

## Backward Compatibility

Composer is fully backward compatible with v1. You can use v1 statements as the starting point for composer composition:

```common-lisp
;; Start with a v1 statement
(defvar *v1-query*
  (select (:id :name)
    (from :users)
    (where (:= :active 1))))

;; Extend it with composer
(-> *v1-query*
    (where (:like :name "%Alice%"))
    (order-by :created_at)
    (limit 10))

;; The v1 query remains unchanged
(yield *v1-query*)
;=> "SELECT id, name FROM users WHERE (active = ?)" (1)
```

## Multiple WHERE Clauses

When you add multiple WHERE clauses, they are automatically combined with AND:

```common-lisp
(-> (from :users)
    (where (:= :active 1))
    (where (:> :age 18))
    (where (:< :age 65)))

;; Yields:
;=> "SELECT * FROM users WHERE ((active = ?) AND ((age > ?) AND (age < ?)))"
;   (1 18 65)
```

## Dynamic Query Building

The `->` macro skips forms that evaluate to NIL, enabling clean conditional composition:

```common-lisp
(defun find-users (&key active role min-age search)
  (-> (from :users)
      (when active (where (:= :active 1)))
      (when role (where (:= :role role)))
      (when min-age (where (:>= :age min-age)))
      (when search (where (:like :name (format nil "%~A%" search))))
      (order-by :name)))

;; Usage
(find-users :active t :role "admin" :min-age 18)
;=> SELECT * FROM users WHERE (((active = ?) AND (role = ?)) AND (age >= ?)) ORDER BY name
;   (1 "admin" 18)

(find-users :search "Alice")
;=> SELECT * FROM users WHERE (name LIKE ?) ORDER BY name
;   ("%Alice%")

(find-users)  ; No filters
;=> SELECT * FROM users ORDER BY name
;   NIL
```

## Converting to SQL

Use the standard `yield` function to generate SQL from a query-state:

```common-lisp
(multiple-value-bind (sql params)
    (yield (-> (from :users)
               (where (:= :active 1))
               (limit 10)))
  (format t "SQL: ~A~%" sql)
  (format t "Params: ~A~%" params))

;; Output:
;; SQL: SELECT * FROM users WHERE (active = ?) LIMIT 10
;; Params: (1)
```

Or use `sql-compile` to get a compiled object:

```common-lisp
(sql-compile
  (-> (from :users)
      (where (:= :id 123))))

;=> #<SXQL-COMPILED: SELECT * FROM users WHERE (id = ?) [123]>
```

## Complete Examples

### Complex SELECT

```common-lisp
(-> (select :u.id :u.name :u.email (:as :p.title :profile_title))
    (from (:as :users :u))
    (left-join (:as :profiles :p) :on (:= :u.id :p.user_id))
    (where (:and (:= :u.active 1)
                 (:> :u.created_at "2024-01-01")))
    (order-by (:desc :u.created_at))
    (limit 50)
    (offset 100))
```

### Conditional UPDATE

```common-lisp
(defun update-user-status (user-id new-status &optional reason)
  (let ((query (-> (update :users)
                   (set= :status new-status :updated_at (local-time:now)))))

    (when reason
      (setf query (-> query (set= :status_reason reason))))

    (-> query
        (where (:= :id user-id))
        (returning :id :status :updated_at))))
```

### Bulk DELETE

```common-lisp
(-> (delete-from :audit_logs)
    (where (:and (:< :created_at "2020-01-01")
                 (:= :severity "info")))
    (order-by :created_at)
    (limit 10000))
```

### INSERT with RETURNING

```common-lisp
(-> (insert-into :users)
    (set= :name "Bob Smith"
          :email "bob@example.com"
          :role "user"
          :active 1)
    (returning :id :created_at))
```

## Migration Guide from v1

### Before (v1)

```common-lisp
;; Building queries conditionally in v1 is verbose
(defun find-products (category min-price max-price)
  (let ((clauses (list (from :products))))

    (when category
      (push (where (:= :category category)) clauses))

    (when min-price
      (push (where (:>= :price min-price)) clauses))

    (when max-price
      (push (where (:<= :price max-price)) clauses))

    (apply #'select :* clauses)))
```

### After (composer)

```common-lisp
;; Much cleaner with composer
(defun find-products (category min-price max-price)
  (let ((query (-> (from :products))))

    (when category
      (setf query (-> query (where (:= :category category)))))

    (when min-price
      (setf query (-> query (where (:>= :price min-price)))))

    (when max-price
      (setf query (-> query (where (:<= :price max-price)))))

    query))
```

Or even more concisely:

```common-lisp
(defun find-products (category min-price max-price)
  (cond-> (-> (from :products))
    category (-> (where (:= :category category)))
    min-price (-> (where (:>= :price min-price)))
    max-price (-> (where (:<= :price max-price)))))
```

## API Reference

### Main Functions

#### `-> (value &rest forms)`

Thread-first macro for query composition. Takes an initial value (statement or clause) and threads it through subsequent clause forms.

**Arguments:**
- `value` - Initial statement or clause
- `forms` - Clause forms to apply

**Returns:** A query-state object

#### `add-clause (query clause)`

Add a clause to a query-state. This is the low-level function used by `->`.

**Arguments:**
- `query` - A query-state object
- `clause` - A clause to add

**Returns:** The modified query-state

### Type Predicates

- `select-query-state-p (object)` - Test if object is a SELECT query-state
- `insert-query-state-p (object)` - Test if object is an INSERT query-state
- `update-query-state-p (object)` - Test if object is an UPDATE query-state
- `delete-query-state-p (object)` - Test if object is a DELETE query-state

### Accessors

All query-state types have accessors for their slots following the naming pattern `<type>-<slot>`:

**SELECT accessors:**
- `select-query-state-fields`
- `select-query-state-where-clauses`
- `select-query-state-order-by-clauses`
- `select-query-state-group-by-clauses`
- `select-query-state-having-clauses`
- `select-query-state-join-clauses`
- `select-query-state-limit-clause`
- `select-query-state-offset-clause`

**INSERT accessors:**
- `insert-query-state-columns`
- `insert-query-state-values-list`
- `insert-query-state-set-clause`
- `insert-query-state-select-subquery`
- `insert-query-state-on-duplicate-key-clause`
- `insert-query-state-on-conflict-clause`

**UPDATE accessors:**
- `update-query-state-set-clause`
- `update-query-state-where-clauses`
- `update-query-state-order-by-clauses`
- `update-query-state-join-clauses`
- `update-query-state-limit-clause`

**DELETE accessors:**
- `delete-query-state-where-clauses`
- `delete-query-state-order-by-clauses`
- `delete-query-state-join-clauses`
- `delete-query-state-limit-clause`

**Common accessors (all types):**
- `query-state-base-primary-table`
- `query-state-base-returning-clause`

## Best Practices

### 1. Use Immutability for Base Queries

Create reusable base queries and derive specific variants:

```common-lisp
(defvar *active-users*
  (-> (from :users)
      (where (:= :active 1))))

(defvar *admin-query*
  (-> *active-users*
      (where (:= :role "admin"))))
```

### 2. Build Queries Incrementally

Let-bind intermediate states for clarity:

```common-lisp
(let* ((base (-> (from :orders)))
       (with-user (-> base
                      (inner-join :users :on (:= :orders.user_id :users.id))))
       (filtered (-> with-user
                     (where (:= :status "pending"))
                     (where (:> :created_at cutoff-date)))))
  (-> filtered
      (order-by (:desc :created_at))
      (limit 100)))
```

### 3. Combine with v1 for One-Off Queries

For simple, non-dynamic queries, v1 syntax is often more concise:

```common-lisp
;; Simple query - v1 is fine
(select (:id :name)
  (from :users)
  (where (:= :id 123)))

;; Dynamic query - use composer
(defun find-users (filters)
  (reduce (lambda (q filter)
            (-> q (where filter)))
          filters
          :initial-value (-> (from :users))))
```

## Performance Considerations

- Composer creates a single copy at the start of `->`, then mutates destructively
- This is more efficient than building nested function calls
- For very hot paths, consider pre-building queries and storing them
- Use `sql-compile` to cache compiled SQL strings

## See Also

- [Main README](README.markdown) - v1 API documentation
- [Source: src/composer.lisp](src/composer.lisp) - Implementation details
- [Tests: test/composer.lisp](test/composer.lisp) - SELECT examples
- [Tests: test/composer-multi.lisp](test/composer-multi.lisp) - INSERT/UPDATE/DELETE examples

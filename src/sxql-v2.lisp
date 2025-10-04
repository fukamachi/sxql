(defpackage #:sxql/v2
  (:use #:cl)
  (:local-nicknames
   (#:a #:alexandria)
   (#:type #:sxql/sql-type)
   (#:clause #:sxql/clause)
   (#:op #:sxql/operator)
   (#:stmt #:sxql/statement))
  (:export ;; v2 Core API
           #:add-clause
           ;; Base type
           #:query-state-base
           #:query-state-base-p
           #:query-state-base-primary-table
           #:query-state-base-returning-clause
           ;; SELECT query state
           #:select-query-state
           #:select-query-state-p
           #:make-select-query-state
           #:select-query-state-fields
           #:select-query-state-where-clauses
           #:select-query-state-order-by-clauses
           #:select-query-state-group-by-clauses
           #:select-query-state-having-clauses
           #:select-query-state-join-clauses
           #:select-query-state-limit-clause
           #:select-query-state-offset-clause
           ;; INSERT query state
           #:insert-query-state
           #:insert-query-state-p
           #:make-insert-query-state
           #:insert-query-state-columns
           #:insert-query-state-values-list
           #:insert-query-state-set-clause
           #:insert-query-state-select-subquery
           #:insert-query-state-on-duplicate-key-clause
           #:insert-query-state-on-conflict-clause
           ;; UPDATE query state
           #:update-query-state
           #:update-query-state-p
           #:make-update-query-state
           #:update-query-state-set-clause
           #:update-query-state-where-clauses
           #:update-query-state-order-by-clauses
           #:update-query-state-join-clauses
           #:update-query-state-limit-clause
           ;; DELETE query state
           #:delete-query-state
           #:delete-query-state-p
           #:make-delete-query-state
           #:delete-query-state-where-clauses
           #:delete-query-state-order-by-clauses
           #:delete-query-state-join-clauses
           #:delete-query-state-limit-clause
           ;; Backward compatibility
           #:query-state
           #:query-state-to-select-statement
           ;; Global column mapping
           #:*column-table-mapping*
           #:register-table-columns
           #:find-column-table
           #:clear-column-mappings
           ;; Threading utilities
           #:->))
(in-package #:sxql/v2)

;;
;; v2 Core Object System
;;

(defstruct query-state-base
  "Base container for all query types"
  (primary-table nil :type (or null string))
  (returning-clause nil))

(defstruct (select-query-state (:include query-state-base))
  "Container for SELECT query clauses"
  (fields nil :type list)
  (where-clauses nil :type list)
  (order-by-clauses nil :type list)
  (group-by-clauses nil :type list)
  (having-clauses nil :type list)
  (join-clauses nil :type list)
  (limit-clause nil)
  (offset-clause nil))

(defstruct (insert-query-state (:include query-state-base))
  "Container for INSERT query clauses"
  (columns nil :type list)
  (values-list nil :type list)
  (set-clause nil)
  (select-subquery nil)
  (on-duplicate-key-clause nil)
  (on-conflict-clause nil))

(defstruct (update-query-state (:include query-state-base))
  "Container for UPDATE query clauses"
  (set-clause nil)
  (where-clauses nil :type list)
  (order-by-clauses nil :type list)
  (join-clauses nil :type list)
  (limit-clause nil))

(defstruct (delete-query-state (:include query-state-base))
  "Container for DELETE query clauses"
  (where-clauses nil :type list)
  (order-by-clauses nil :type list)
  (join-clauses nil :type list)
  (limit-clause nil))

;; Backward compatibility alias
(deftype query-state () 'select-query-state)

;;
;; v2 Composition Functions
;;

(defun add-where-clause (query clause)
  "Add a WHERE clause to a query state (destructive)"
  (let ((where-clause (typecase clause
                        ;; If it's already a WHERE clause, use it
                        (sxql.clause::where-clause clause)
                        ;; Otherwise create a WHERE clause from the expression
                        (otherwise (clause:make-clause :where clause)))))
    (etypecase query
      (select-query-state
       (push where-clause (select-query-state-where-clauses query)))
      (update-query-state
       (push where-clause (update-query-state-where-clauses query)))
      (delete-query-state
       (push where-clause (delete-query-state-where-clauses query))))
    query))

(defun add-order-by-clause (query clause)
  "Add an ORDER BY clause to a query state (destructive)"
  (let ((order-clause (typecase clause
                        ;; If it's already an ORDER BY clause, use it
                        (sxql.clause::order-by-clause clause)
                        ;; Otherwise create an ORDER BY clause from the expression
                        (otherwise (clause:make-clause :order-by clause)))))
    (etypecase query
      (select-query-state
       (push order-clause (select-query-state-order-by-clauses query)))
      (update-query-state
       (push order-clause (update-query-state-order-by-clauses query)))
      (delete-query-state
       (push order-clause (delete-query-state-order-by-clauses query))))
    query))

(defun add-group-by-clause (query clause)
  "Add a GROUP BY clause to a query state (destructive)"
  (let ((group-clause (typecase clause
                        (sxql.clause::group-by-clause clause)
                        (otherwise (clause:make-clause :group-by clause)))))
    (etypecase query
      (select-query-state
       (push group-clause (select-query-state-group-by-clauses query))))
    query))

(defun add-having-clause (query clause)
  "Add a HAVING clause to a query state (destructive)"
  (let ((having-clause (typecase clause
                         (sxql.clause::having-clause clause)
                         (otherwise (clause:make-clause :having clause)))))
    (etypecase query
      (select-query-state
       (push having-clause (select-query-state-having-clauses query))))
    query))

(defun add-join-clause (query clause)
  "Add a JOIN clause to a query state with automatic column qualification (destructive)"
  (let ((join-clause (typecase clause
                       (sxql.clause::join-clause clause)
                       (otherwise clause))))  ; Join clauses are more complex, pass through

    ;; Add the JOIN clause and check for auto-qualification
    (etypecase query
      (select-query-state
       (push join-clause (select-query-state-join-clauses query))
       ;; Auto-qualification when first JOIN is added
       (when (and (= 1 (length (select-query-state-join-clauses query)))
                  (query-state-base-primary-table query))
         (qualify-all-clauses query (query-state-base-primary-table query))))
      (update-query-state
       (push join-clause (update-query-state-join-clauses query))
       (when (and (= 1 (length (update-query-state-join-clauses query)))
                  (query-state-base-primary-table query))
         (qualify-all-clauses query (query-state-base-primary-table query))))
      (delete-query-state
       (push join-clause (delete-query-state-join-clauses query))
       (when (and (= 1 (length (delete-query-state-join-clauses query)))
                  (query-state-base-primary-table query))
         (qualify-all-clauses query (query-state-base-primary-table query)))))

    query))

(defun add-fields-clause (query clause)
  "Add a FIELDS clause to a query state (destructive)"
  (let ((fields-clause (typecase clause
                         (sxql.clause::fields-clause clause)
                         (otherwise (clause:make-clause :fields clause)))))
    (etypecase query
      (select-query-state
       (a:appendf (select-query-state-fields query)
                  (type::sql-splicing-list-elements
                   (clause::fields-clause-statement fields-clause)))))
    query))

(defun add-from-clause (query clause)
  "Add a FROM clause to a query state (destructive)"
  (let ((from-clause (typecase clause
                       (sxql.clause::from-clause clause)
                       (otherwise (clause:make-clause :from clause)))))
    (setf (query-state-base-primary-table query)
          (clause:from-clause-table-name from-clause))
    query))

(defun add-limit-clause (query clause)
  "Add a LIMIT clause to a query state (destructive)"
  (let ((limit-clause (typecase clause
                        (sxql.clause::limit-clause clause)
                        (otherwise (clause:make-clause :limit clause)))))
    (etypecase query
      (select-query-state
       (setf (select-query-state-limit-clause query) limit-clause))
      (update-query-state
       (setf (update-query-state-limit-clause query) limit-clause))
      (delete-query-state
       (setf (delete-query-state-limit-clause query) limit-clause)))
    query))

(defun add-offset-clause (query clause)
  "Add an OFFSET clause to a query state (destructive)"
  (let ((offset-clause (typecase clause
                         (sxql.clause::offset-clause clause)
                         (otherwise (clause:make-clause :offset clause)))))
    (etypecase query
      (select-query-state
       (setf (select-query-state-offset-clause query) offset-clause)))
    query))

(defun add-returning-clause (query clause)
  "Add a RETURNING clause to a query state (destructive)"
  (let ((returning-clause (typecase clause
                            (sxql.clause::returning-clause clause)
                            (otherwise (clause:make-clause :returning clause)))))
    (setf (query-state-base-returning-clause query) returning-clause)
    query))

(defun add-set=-clause (query clause)
  "Add a SET= clause to UPDATE or INSERT query state (destructive)"
  (let ((set-clause (typecase clause
                      (sxql.clause::set=-clause clause)
                      (otherwise clause))))
    (etypecase query
      (update-query-state
       (setf (update-query-state-set-clause query) set-clause))
      (insert-query-state
       (setf (insert-query-state-set-clause query) set-clause)))
    query))

(defun add-values-clause (query clause)
  "Add a VALUES clause to INSERT query state (destructive)"
  (let ((values-clause (typecase clause
                         (sxql.clause::values-clause clause)
                         (otherwise clause))))
    (etypecase query
      (insert-query-state
       (push values-clause (insert-query-state-values-list query))))
    query))

(defun add-on-duplicate-key-update-clause (query clause)
  "Add ON DUPLICATE KEY UPDATE clause to INSERT query state (destructive)"
  (etypecase query
    (insert-query-state
     (setf (insert-query-state-on-duplicate-key-clause query) clause)))
  query)

(defun add-on-conflict-clause (query clause)
  "Add ON CONFLICT clause to INSERT query state (destructive)"
  (etypecase query
    (insert-query-state
     (setf (insert-query-state-on-conflict-clause query) clause)))
  query)

;;
;; v2 Helper Functions
;;

(defun copy-query-state-immutable (query)
  "Create a copy of a query-state for immutable updates"
  (etypecase query
    (select-query-state
     (make-select-query-state
      :primary-table (query-state-base-primary-table query)
      :returning-clause (query-state-base-returning-clause query)
      :fields (copy-list (select-query-state-fields query))
      :where-clauses (copy-list (select-query-state-where-clauses query))
      :order-by-clauses (copy-list (select-query-state-order-by-clauses query))
      :group-by-clauses (copy-list (select-query-state-group-by-clauses query))
      :having-clauses (copy-list (select-query-state-having-clauses query))
      :join-clauses (copy-list (select-query-state-join-clauses query))
      :limit-clause (select-query-state-limit-clause query)
      :offset-clause (select-query-state-offset-clause query)))
    (insert-query-state
     (make-insert-query-state
      :primary-table (query-state-base-primary-table query)
      :returning-clause (query-state-base-returning-clause query)
      :columns (copy-list (insert-query-state-columns query))
      :values-list (copy-list (insert-query-state-values-list query))
      :set-clause (insert-query-state-set-clause query)
      :select-subquery (insert-query-state-select-subquery query)
      :on-duplicate-key-clause (insert-query-state-on-duplicate-key-clause query)
      :on-conflict-clause (insert-query-state-on-conflict-clause query)))
    (update-query-state
     (make-update-query-state
      :primary-table (query-state-base-primary-table query)
      :returning-clause (query-state-base-returning-clause query)
      :set-clause (update-query-state-set-clause query)
      :where-clauses (copy-list (update-query-state-where-clauses query))
      :order-by-clauses (copy-list (update-query-state-order-by-clauses query))
      :join-clauses (copy-list (update-query-state-join-clauses query))
      :limit-clause (update-query-state-limit-clause query)))
    (delete-query-state
     (make-delete-query-state
      :primary-table (query-state-base-primary-table query)
      :returning-clause (query-state-base-returning-clause query)
      :where-clauses (copy-list (delete-query-state-where-clauses query))
      :order-by-clauses (copy-list (delete-query-state-order-by-clauses query))
      :join-clauses (copy-list (delete-query-state-join-clauses query))
      :limit-clause (delete-query-state-limit-clause query)))))

;;
;; v2 Global Column Mapping System
;;

(defvar *column-table-mapping* (make-hash-table :test #'equal)
  "Global hash table mapping column names to their primary table names.
   Key: column name (string), Value: table name (string).
   This provides a reusable mapping across all queries.")

(defun register-table-columns (table-name column-names)
  "Register that a list of columns belong to a table.

   This enables intelligent column qualification during auto-qualification.
   When a JOIN is added to a query, unqualified columns will be qualified
   using this mapping first, falling back to the primary table name.

   Example:
     (register-table-columns \"users\" '(\"id\" \"email\" \"is_active\"))
     (register-table-columns \"posts\" '(\"id\" \"title\" \"author_id\"))"
  (check-type table-name string)
  (check-type column-names list)
  (dolist (column-name column-names)
    (check-type column-name string)
    (setf (gethash column-name *column-table-mapping*) table-name)))

(defun find-column-table (column-name)
  "Find which table a column belongs to using global mapping"
  (check-type column-name string)
  (gethash column-name *column-table-mapping*))


(defun clear-column-mappings ()
  "Clear all global column-to-table mappings"
  (clrhash *column-table-mapping*))

;;
;; v2 Column Qualification System
;;

(defun is-qualified-column (sql-symbol)
  "Check if SQL symbol is already qualified (contains '.')"
  (when (type:sql-symbol-p sql-symbol)
    (find #\. (type:sql-symbol-name sql-symbol))))

(defun qualify-sql-symbol (sql-symbol primary-table-name)
  "Transform unqualified SQL symbol to qualified one using global mapping or primary table"
  (when sql-symbol
    (let* ((symbol-name (type:sql-symbol-name sql-symbol))
           ;; Try global mapping first, fallback to primary table
           (table-name (or (find-column-table symbol-name) primary-table-name)))
      (when table-name
        (type:make-sql-symbol (format nil "~A.~A" table-name symbol-name))))))


(defun qualify-expression (expression table-name)
  "Recursively qualify unqualified columns in an expression"
  (cond
    ;; SQL symbol - qualify if unqualified
    ((type:sql-symbol-p expression)
     (if (is-qualified-column expression)
         expression
         (qualify-sql-symbol expression table-name)))
    ;; Operators - recursively qualify operands
    ((typep expression 'type:infix-op)
     (op:make-op (intern (type::sql-op-name expression) :keyword)
                 (qualify-expression (type::infix-op-left expression) table-name)
                 (qualify-expression (type::infix-op-right expression) table-name)))
    ((typep expression 'type:unary-op)
     (op:make-op (intern (type::sql-op-name expression) :keyword)
                 (qualify-expression (type::unary-op-var expression) table-name)))
    ((typep expression 'type:conjunctive-op)
     (apply #'op:make-op (intern (type::sql-op-name expression) :keyword)
            (mapcar (lambda (expr) (qualify-expression expr table-name))
                    (type::conjunctive-op-expressions expression))))
    ;; Lists - qualify elements
    ((listp expression)
     (mapcar (lambda (expr) (qualify-expression expr table-name)) expression))
    ;; Everything else - return as-is
    (t expression)))

;;
;; v2 Clause Transformation System
;;

(defun get-where-clauses (query)
  "Get WHERE clauses from any query-state type"
  (etypecase query
    (select-query-state (select-query-state-where-clauses query))
    (update-query-state (update-query-state-where-clauses query))
    (delete-query-state (delete-query-state-where-clauses query))))

(defun (setf get-where-clauses) (value query)
  "Set WHERE clauses for any query-state type"
  (etypecase query
    (select-query-state (setf (select-query-state-where-clauses query) value))
    (update-query-state (setf (update-query-state-where-clauses query) value))
    (delete-query-state (setf (delete-query-state-where-clauses query) value))))

(defun get-order-by-clauses (query)
  "Get ORDER BY clauses from any query-state type"
  (etypecase query
    (select-query-state (select-query-state-order-by-clauses query))
    (update-query-state (update-query-state-order-by-clauses query))
    (delete-query-state (delete-query-state-order-by-clauses query))))

(defun (setf get-order-by-clauses) (value query)
  "Set ORDER BY clauses for any query-state type"
  (etypecase query
    (select-query-state (setf (select-query-state-order-by-clauses query) value))
    (update-query-state (setf (update-query-state-order-by-clauses query) value))
    (delete-query-state (setf (delete-query-state-order-by-clauses query) value))))

(defun qualify-where-clauses (query table-name)
  "Transform WHERE clauses to qualify unqualified columns"
  (when (get-where-clauses query)
    (setf (get-where-clauses query)
          (mapcar (lambda (where-clause)
                    (let ((expr (if (typep where-clause 'sxql.clause::where-clause)
                                   (slot-value where-clause 'sxql.clause::expression)
                                   where-clause)))
                      (clause:make-clause :where (qualify-expression expr table-name))))
                  (get-where-clauses query))))
  query)

(defun qualify-order-by-clauses (query table-name)
  "Transform ORDER BY clauses to qualify unqualified columns"
  (when (get-order-by-clauses query)
    (setf (get-order-by-clauses query)
          (mapcar (lambda (order-clause)
                    (let ((expressions (slot-value order-clause 'type:expressions)))
                      (apply #'clause:make-clause :order-by
                             (mapcar (lambda (expr) (qualify-expression expr table-name))
                                     expressions))))
                  (get-order-by-clauses query))))
  query)

(defun qualify-having-clauses (query table-name)
  "Transform HAVING clauses to qualify unqualified columns"
  (etypecase query
    (select-query-state
     (when (select-query-state-having-clauses query)
       (setf (select-query-state-having-clauses query)
             (mapcar (lambda (having-clause)
                       (let ((expr (if (typep having-clause 'sxql.clause::having-clause)
                                      (slot-value having-clause 'sxql.clause::expression)
                                      having-clause)))
                         (clause:make-clause :having (qualify-expression expr table-name))))
                     (select-query-state-having-clauses query))))))
  query)

(defun qualify-all-clauses (query table-name)
  "Apply column qualification to all relevant clauses in a query"
  (qualify-where-clauses query table-name)
  (qualify-order-by-clauses query table-name)
  (qualify-having-clauses query table-name)
  query)

;;
;; v2 SQL Generation
;;

(defun query-state-to-select-statement (query)
  "Convert a select-query-state object to a proper SxQL select-statement"
  (check-type query select-query-state)

  (let ((clauses '()))

    ;; Add FIELDS clause - use :* if no fields specified
    (if (select-query-state-fields query)
        (push (clause:make-clause :fields
                                  (apply #'type:make-sql-splicing-list
                                         (reverse (select-query-state-fields query))))
              clauses)
        (push (clause:make-clause :fields :*) clauses))

    ;; Add FROM clause
    (when (query-state-base-primary-table query)
      (let ((table-name (query-state-base-primary-table query)))
        (push (clause:make-clause :from
                                  (if (symbolp table-name)
                                      (type:make-sql-symbol (string-downcase table-name))
                                      (type:make-sql-symbol table-name)))
              clauses)))

    ;; Add WHERE clauses - combine multiple WHERE clauses with AND
    (when (select-query-state-where-clauses query)
      (let ((where-clauses (reverse (select-query-state-where-clauses query))))
        (if (= 1 (length where-clauses))
            ;; Single WHERE clause
            (push (first where-clauses) clauses)
            ;; Multiple WHERE clauses - manually combine with AND
            ;; Extract expressions from WHERE clauses and combine with AND operator
            (let ((where-expressions '()))
              (dolist (where-clause where-clauses)
                ;; For WHERE clauses created by sxql:where, we need to extract the expression
                ;; We'll use the internal structure to get the expression
                (let ((expr (if (typep where-clause 'sxql.clause::where-clause)
                               ;; This is a proper WHERE clause - extract its expression
                               (slot-value where-clause 'sxql.clause::expression)
                               ;; Fallback
                               where-clause)))
                  (push expr where-expressions)))
              ;; Create a new WHERE clause with all expressions combined with AND
              (push (clause:make-clause :where
                                        (if (= 1 (length where-expressions))
                                            (first where-expressions)
                                            (apply #'op:make-op :and (reverse where-expressions))))
                    clauses)))))

    ;; Add ORDER BY clauses - combine multiple clauses into one
    (let ((order-by-clauses (select-query-state-order-by-clauses query)))
      (when order-by-clauses
        (if (= 1 (length order-by-clauses))
            ;; Single ORDER BY clause
            (push (first (reverse order-by-clauses)) clauses)
            ;; Multiple ORDER BY clauses - combine expressions into one clause
            (let ((order-expressions (loop for clause in (reverse order-by-clauses)
                                          append (slot-value clause 'type:expressions))))
              (push (apply #'clause:make-clause :order-by order-expressions) clauses)))))

    ;; Add GROUP BY clauses
    (dolist (clause (reverse (select-query-state-group-by-clauses query)))
      (push clause clauses))

    ;; Add HAVING clauses
    (dolist (clause (reverse (select-query-state-having-clauses query)))
      (push clause clauses))

    ;; Add JOIN clauses
    (dolist (clause (reverse (select-query-state-join-clauses query)))
      (push clause clauses))

    ;; Add LIMIT clause
    (when (select-query-state-limit-clause query)
      (push (select-query-state-limit-clause query) clauses))

    ;; Add OFFSET clause
    (when (select-query-state-offset-clause query)
      (push (select-query-state-offset-clause query) clauses))

    ;; Add RETURNING clause
    (when (query-state-base-returning-clause query)
      (push (query-state-base-returning-clause query) clauses))

    ;; Create the select statement
    (apply #'stmt:make-statement :select (reverse clauses))))

(defun query-state-to-insert-statement (query)
  "Convert an insert-query-state to a proper SxQL insert-into-statement"
  (check-type query insert-query-state)
  (let ((clauses '()))
    ;; Add table name
    (when (query-state-base-primary-table query)
      (push (type:make-sql-symbol (query-state-base-primary-table query)) clauses))

    ;; Add VALUES clauses
    (dolist (values-clause (reverse (insert-query-state-values-list query)))
      (push values-clause clauses))

    ;; Add SET= clause
    (when (insert-query-state-set-clause query)
      (push (insert-query-state-set-clause query) clauses))

    ;; Add RETURNING clause
    (when (query-state-base-returning-clause query)
      (push (query-state-base-returning-clause query) clauses))

    ;; Add ON DUPLICATE KEY UPDATE clause
    (when (insert-query-state-on-duplicate-key-clause query)
      (push (insert-query-state-on-duplicate-key-clause query) clauses))

    ;; Add ON CONFLICT clause
    (when (insert-query-state-on-conflict-clause query)
      (push (insert-query-state-on-conflict-clause query) clauses))

    (apply #'stmt:make-statement :insert-into (reverse clauses))))

(defun combine-where-clauses (where-clauses-list)
  "Combine multiple WHERE clauses with AND"
  (when where-clauses-list
    (let ((where-clauses (reverse where-clauses-list)))
      (if (= 1 (length where-clauses))
          (first where-clauses)
          (let ((where-expressions '()))
            (dolist (where-clause where-clauses)
              (let ((expr (if (typep where-clause 'sxql.clause::where-clause)
                             (slot-value where-clause 'sxql.clause::expression)
                             where-clause)))
                (push expr where-expressions)))
            (clause:make-clause :where
                                (if (= 1 (length where-expressions))
                                    (first where-expressions)
                                    (apply #'op:make-op :and (reverse where-expressions)))))))))

(defun combine-order-by-clauses (order-by-clauses-list)
  "Combine multiple ORDER BY clauses into one"
  (when order-by-clauses-list
    (if (= 1 (length order-by-clauses-list))
        (first (reverse order-by-clauses-list))
        (let ((order-expressions (loop for clause in (reverse order-by-clauses-list)
                                      append (slot-value clause 'type:expressions))))
          (apply #'clause:make-clause :order-by order-expressions)))))

(defun query-state-to-update-statement (query)
  "Convert an update-query-state to a proper SxQL update-statement"
  (check-type query update-query-state)
  (let ((clauses '()))
    ;; Add table name
    (when (query-state-base-primary-table query)
      (push (type:make-sql-symbol (query-state-base-primary-table query)) clauses))
    ;; Add SET= clause
    (when (update-query-state-set-clause query)
      (push (update-query-state-set-clause query) clauses))
    ;; Add WHERE clauses
    (a:when-let ((where (combine-where-clauses (update-query-state-where-clauses query))))
      (push where clauses))
    ;; Add ORDER BY clauses
    (a:when-let ((order-by (combine-order-by-clauses (update-query-state-order-by-clauses query))))
      (push order-by clauses))
    ;; Add JOIN clauses
    (dolist (clause (reverse (update-query-state-join-clauses query)))
      (push clause clauses))
    ;; Add LIMIT clause
    (when (update-query-state-limit-clause query)
      (push (update-query-state-limit-clause query) clauses))
    ;; Add RETURNING clause
    (when (query-state-base-returning-clause query)
      (push (query-state-base-returning-clause query) clauses))
    (apply #'stmt:make-statement :update (reverse clauses))))

(defun query-state-to-delete-statement (query)
  "Convert a delete-query-state to a proper SxQL delete-from-statement"
  (check-type query delete-query-state)
  (let ((clauses '()))
    ;; Add table name
    (when (query-state-base-primary-table query)
      (push (type:make-sql-symbol (query-state-base-primary-table query)) clauses))
    ;; Add WHERE clauses
    (a:when-let ((where (combine-where-clauses (delete-query-state-where-clauses query))))
      (push where clauses))
    ;; Add ORDER BY clauses
    (a:when-let ((order-by (combine-order-by-clauses (delete-query-state-order-by-clauses query))))
      (push order-by clauses))
    ;; Add JOIN clauses
    (dolist (clause (reverse (delete-query-state-join-clauses query)))
      (push clause clauses))
    ;; Add LIMIT clause
    (when (delete-query-state-limit-clause query)
      (push (delete-query-state-limit-clause query) clauses))
    ;; Add RETURNING clause
    (when (query-state-base-returning-clause query)
      (push (query-state-base-returning-clause query) clauses))
    (apply #'stmt:make-statement :delete-from (reverse clauses))))

(defun yield-query (query)
  "Generate SQL string and parameters from a query-state object using SxQL infrastructure"
  (etypecase query
    (select-query-state
     (let ((select-stmt (query-state-to-select-statement query)))
       (multiple-value-bind (sql params)
           (type:with-yield-binds
             (type:yield select-stmt))
         (cons sql params))))
    (insert-query-state
     (let ((insert-stmt (query-state-to-insert-statement query)))
       (multiple-value-bind (sql params)
           (type:with-yield-binds
             (type:yield insert-stmt))
         (cons sql params))))
    (update-query-state
     (let ((update-stmt (query-state-to-update-statement query)))
       (multiple-value-bind (sql params)
           (type:with-yield-binds
             (type:yield update-stmt))
         (cons sql params))))
    (delete-query-state
     (let ((delete-stmt (query-state-to-delete-statement query)))
       (multiple-value-bind (sql params)
           (type:with-yield-binds
             (type:yield delete-stmt))
         (cons sql params))))))

;; Add methods to sxql:yield so users can use it for both v1 and v2 queries
(defmethod type:yield ((query select-query-state))
  "Allow sxql:yield to work with select-query-state objects"
  (let ((result (yield-query query)))
    (values (car result) (cdr result))))

(defmethod type:yield ((query insert-query-state))
  "Allow sxql:yield to work with insert-query-state objects"
  (let ((result (yield-query query)))
    (values (car result) (cdr result))))

(defmethod type:yield ((query update-query-state))
  "Allow sxql:yield to work with update-query-state objects"
  (let ((result (yield-query query)))
    (values (car result) (cdr result))))

(defmethod type:yield ((query delete-query-state))
  "Allow sxql:yield to work with delete-query-state objects"
  (let ((result (yield-query query)))
    (values (car result) (cdr result))))

;;
;; v2 Threading Utilities
;;
;; Note: v2 does not define its own clause macros. Use sxql:where, sxql:order-by,
;; sxql:limit, etc. directly with the -> threading macro. They work seamlessly!
;;

(defun select-statement-to-query-state (select-stmt)
  "Convert a select-statement to a select-query-state for v2 composition"
  (check-type select-stmt stmt:select-statement)
  (let ((query (make-select-query-state)))
    ;; Extract clauses from select-statement using the exported function
    (dolist (clause (stmt:compute-select-statement-children select-stmt))
      (setf query (add-clause query clause)))
    query))

(defun statement-to-query-state (statement query-constructor)
  "Generic converter from statement to query-state"
  (let ((query (funcall query-constructor))
        (children (type:sql-composed-statement-children statement)))
    ;; Extract table name from first child if it's a symbol
    (when (and children (typep (first children) 'type:sql-symbol))
      (setf (query-state-base-primary-table query)
            (type:sql-symbol-name (first children)))
      (setf children (rest children)))
    ;; Extract clauses
    (dolist (clause children)
      (setf query (add-clause query clause)))
    query))

(defun insert-statement-to-query-state (insert-stmt)
  "Convert an insert-into-statement to an insert-query-state for v2 composition"
  (check-type insert-stmt stmt:insert-into-statement)
  (statement-to-query-state insert-stmt #'make-insert-query-state))

(defun update-statement-to-query-state (update-stmt)
  "Convert an update-statement to an update-query-state for v2 composition"
  (check-type update-stmt stmt:update-statement)
  (statement-to-query-state update-stmt #'make-update-query-state))

(defun delete-statement-to-query-state (delete-stmt)
  "Convert a delete-from-statement to a delete-query-state for v2 composition"
  (check-type delete-stmt stmt:delete-from-statement)
  (statement-to-query-state delete-stmt #'make-delete-query-state))

(defgeneric add-clause (query clause)
  (:method ((query query-state-base) clause)
    (etypecase clause
      (sxql.clause::where-clause
       (add-where-clause query clause))
      (sxql.clause::order-by-clause
       (add-order-by-clause query clause))
      (sxql.clause::group-by-clause
       (add-group-by-clause query clause))
      (sxql.clause::having-clause
       (add-having-clause query clause))
      (sxql.clause::join-clause
       (add-join-clause query clause))
      (sxql.clause::fields-clause
       (add-fields-clause query clause))
      (sxql.clause::from-clause
       (add-from-clause query clause))
      (sxql.clause::limit-clause
       (add-limit-clause query clause))
      (sxql.clause::offset-clause
       (add-offset-clause query clause))
      (sxql.clause::returning-clause
       (add-returning-clause query clause))
      (sxql.clause::set=-clause
       (add-set=-clause query clause))
      (sxql.clause::values-clause
       (add-values-clause query clause))
      (sxql.clause::on-duplicate-key-update-clause
       (add-on-duplicate-key-update-clause query clause))
      ((or sxql.clause::on-conflict-do-nothing-clause
           sxql.clause::on-conflict-do-update-clause)
       (add-on-conflict-clause query clause)))))

(defmacro -> (value &rest forms)
  "Smart threading macro that dispatches based on clause types returned by forms.

   This macro enables immutable query composition by threading clauses through
   query-state objects. Creates a single copy at the beginning, then destructively
   adds clauses for efficiency. Supports both v1 select-statements and v2 query-states.

   Examples:
     ;; With v2 clause
     (-> (from :users)
         (where (:= :active 1))
         (order-by :name))

     ;; With v1 select-statement (backward compatible)
     (-> (select (:id :name) (from :users))
         (where (:= :active 1))
         (order-by :name))

     (let ((base-query (select (from :users))))
       (-> base-query
           (where (:= :department \"engineering\"))
           (limit 10)))"
  (a:with-gensyms (threaded-var)
    `(let* ((,threaded-var ,value)
            ;; Convert to query-state and copy once at the beginning
            (,threaded-var
              (typecase ,threaded-var
                (query-state-base (copy-query-state-immutable ,threaded-var))
                (stmt:select-statement (select-statement-to-query-state ,threaded-var))
                (stmt:insert-into-statement (insert-statement-to-query-state ,threaded-var))
                (stmt:update-statement (update-statement-to-query-state ,threaded-var))
                (stmt:delete-from-statement (delete-statement-to-query-state ,threaded-var))
                (otherwise (add-clause (make-select-query-state) ,threaded-var)))))
       ;; Now destructively add clauses (no more copying)
       ,@(loop for form in forms
               collect `(add-clause ,threaded-var
                                    ,(if (listp form)
                                         ;; For list forms, call the function with its arguments (no threading)
                                         `(,(car form) ,@(cdr form))
                                         ;; For symbol forms, call the function with no arguments
                                         `(,form))))
       ,threaded-var)))

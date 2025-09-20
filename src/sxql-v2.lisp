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
           #:query-state
           #:query-state-p
           #:query-state-primary-table
           #:query-state-where-clauses
           #:query-state-order-by-clauses
           #:query-state-group-by-clauses
           #:query-state-having-clauses
           #:query-state-join-clauses
           #:query-state-fields
           #:query-state-limit-clause
           #:query-state-offset-clause
           ;; Global column mapping
           #:*column-table-mapping*
           #:register-table-columns
           #:find-column-table
           #:clear-column-mappings
           #:yield-query
           ;; Query builder
           #:select
           ;; Clause macros
           #:fields
           #:from
           #:where
           #:order-by
           #:group-by
           #:having
           #:returning
           #:join
           #:inner-join
           #:left-join
           #:right-join
           #:full-join
           #:limit
           #:offset
           ;; Threading utilities
           #:->))
(in-package #:sxql/v2)

;;
;; v2 Core Object System
;;

(defstruct query-state
  "Container for query clauses and context"
  (primary-table nil :type (or null string))
  (where-clauses nil :type list)
  (order-by-clauses nil :type list)
  (group-by-clauses nil :type list)
  (having-clauses nil :type list)
  (join-clauses nil :type list)
  (fields nil :type list)
  (limit-clause nil)
  (offset-clause nil)
  (returning-clause nil))

;;
;; v2 Composition Functions
;;

(defun add-where-clause (query clause)
  "Add a WHERE clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (where-clause (typecase clause
                        ;; If it's already a WHERE clause, use it
                        (sxql.clause::where-clause clause)
                        ;; Otherwise create a WHERE clause from the expression
                        (otherwise (clause:make-clause :where clause)))))
    (push where-clause (query-state-where-clauses new-query))
    new-query))

(defun add-order-by-clause (query clause)
  "Add an ORDER BY clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (order-clause (typecase clause
                        ;; If it's already an ORDER BY clause, use it
                        (sxql.clause::order-by-clause clause)
                        ;; Otherwise create an ORDER BY clause from the expression
                        (otherwise (clause:make-clause :order-by clause)))))
    (push order-clause (query-state-order-by-clauses new-query))
    new-query))

(defun add-group-by-clause (query clause)
  "Add a GROUP BY clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (group-clause (typecase clause
                        (sxql.clause::group-by-clause clause)
                        (otherwise (clause:make-clause :group-by clause)))))
    (push group-clause (query-state-group-by-clauses new-query))
    new-query))

(defun add-having-clause (query clause)
  "Add a HAVING clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (having-clause (typecase clause
                         (sxql.clause::having-clause clause)
                         (otherwise (clause:make-clause :having clause)))))
    (push having-clause (query-state-having-clauses new-query))
    new-query))

(defun add-join-clause (query clause)
  "Add a JOIN clause to a query state with automatic column qualification"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (join-clause (typecase clause
                       (sxql.clause::join-clause clause)
                       (otherwise clause))))  ; Join clauses are more complex, pass through

    ;; Add the JOIN clause
    (push join-clause (query-state-join-clauses new-query))

    ;; Auto-qualification when first JOIN is added
    (when (and (= 1 (length (query-state-join-clauses new-query))) ; First JOIN
               (query-state-primary-table new-query))              ; Has primary table
      ;; Qualify existing clauses with primary table name
      (qualify-all-clauses new-query
                           (query-state-primary-table new-query)))

    new-query))

(defun add-fields-clause (query clause)
  "Add a FIELDS clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (fields-clause (typecase clause
                         (sxql.clause::fields-clause clause)
                         (otherwise (clause:make-clause :fields clause)))))
    (a:appendf (query-state-fields new-query)
               (type::sql-splicing-list-elements
                (clause::fields-clause-statement fields-clause)))
    new-query))

(defun add-from-clause (query clause)
  "Add a FROM clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (from-clause (typecase clause
                       (sxql.clause::from-clause clause)
                       (otherwise (clause:make-clause :from clause)))))
    (setf (query-state-primary-table new-query)
          (clause:from-clause-table-name from-clause))
    new-query))

(defun add-limit-clause (query clause)
  "Add a LIMIT clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (limit-clause (typecase clause
                        (sxql.clause::limit-clause clause)
                        (otherwise (clause:make-clause :limit clause)))))
    (setf (query-state-limit-clause new-query) limit-clause)
    new-query))

(defun add-offset-clause (query clause)
  "Add an OFFSET clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (offset-clause (typecase clause
                         (sxql.clause::offset-clause clause)
                         (otherwise (clause:make-clause :offset clause)))))
    (setf (query-state-offset-clause new-query) offset-clause)
    new-query))

(defun add-returning-clause (query clause)
  "Add a RETURNING clause to a query state"
  (check-type query query-state)
  (let ((new-query (copy-query-state-immutable query))
        (returning-clause (typecase clause
                            (sxql.clause::returning-clause clause)
                            (otherwise (clause:make-clause :returning clause)))))
    (setf (query-state-returning-clause new-query) returning-clause)
    new-query))

;;
;; v2 Helper Functions
;;

(defun copy-query-state-immutable (query)
  "Create a copy of a query-state for immutable updates"
  (make-query-state
   :primary-table (query-state-primary-table query)
   :where-clauses (copy-list (query-state-where-clauses query))
   :order-by-clauses (copy-list (query-state-order-by-clauses query))
   :group-by-clauses (copy-list (query-state-group-by-clauses query))
   :having-clauses (copy-list (query-state-having-clauses query))
   :join-clauses (copy-list (query-state-join-clauses query))
   :fields (copy-list (query-state-fields query))
   :limit-clause (query-state-limit-clause query)
   :offset-clause (query-state-offset-clause query)
   :returning-clause (query-state-returning-clause query)))

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

(defun qualify-where-clauses (query table-name)
  "Transform WHERE clauses to qualify unqualified columns"
  (when (query-state-where-clauses query)
    (setf (query-state-where-clauses query)
          (mapcar (lambda (where-clause)
                    ;; Extract expression from WHERE clause and qualify it
                    (let ((expr (if (typep where-clause 'sxql.clause::where-clause)
                                   (slot-value where-clause 'sxql.clause::expression)
                                   where-clause)))
                      (clause:make-clause :where (qualify-expression expr table-name))))
                  (query-state-where-clauses query))))
  query)

(defun qualify-order-by-clauses (query table-name)
  "Transform ORDER BY clauses to qualify unqualified columns"
  (when (query-state-order-by-clauses query)
    (setf (query-state-order-by-clauses query)
          (mapcar (lambda (order-clause)
                    ;; Extract expressions from ORDER BY clause and qualify them
                    (let ((expressions (slot-value order-clause 'type:expressions)))
                      (apply #'clause:make-clause :order-by
                             (mapcar (lambda (expr) (qualify-expression expr table-name))
                                     expressions))))
                  (query-state-order-by-clauses query))))
  query)

(defun qualify-having-clauses (query table-name)
  "Transform HAVING clauses to qualify unqualified columns"
  (when (query-state-having-clauses query)
    (setf (query-state-having-clauses query)
          (mapcar (lambda (having-clause)
                    ;; Extract expression from HAVING clause and qualify it
                    (let ((expr (if (typep having-clause 'sxql.clause::having-clause)
                                   (slot-value having-clause 'sxql.clause::expression)
                                   having-clause)))
                      (clause:make-clause :having (qualify-expression expr table-name))))
                  (query-state-having-clauses query))))
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
  "Convert a query-state object to a proper SxQL select-statement"
  (check-type query query-state)

  (let ((clauses '()))

    ;; Add FIELDS clause - use :* if no fields specified
    (if (query-state-fields query)
        (push (clause:make-clause :fields
                                  (apply #'type:make-sql-splicing-list
                                         (reverse (query-state-fields query))))
              clauses)
        (push (clause:make-clause :fields :*) clauses))

    ;; Add FROM clause
    (when (query-state-primary-table query)
      (let ((table-name (query-state-primary-table query)))
        (push (clause:make-clause :from
                                  (if (symbolp table-name)
                                      (type:make-sql-symbol (string-downcase table-name))
                                      (type:make-sql-symbol table-name)))
              clauses)))

    ;; Add WHERE clauses - combine multiple WHERE clauses with AND
    (when (query-state-where-clauses query)
      (let ((where-clauses (reverse (query-state-where-clauses query))))
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
    (let ((order-by-clauses (query-state-order-by-clauses query)))
      (when order-by-clauses
        (if (= 1 (length order-by-clauses))
            ;; Single ORDER BY clause
            (push (first (reverse order-by-clauses)) clauses)
            ;; Multiple ORDER BY clauses - combine expressions into one clause
            (let ((order-expressions (loop for clause in (reverse order-by-clauses)
                                          append (slot-value clause 'type:expressions))))
              (push (apply #'clause:make-clause :order-by order-expressions) clauses)))))

    ;; Add GROUP BY clauses
    (dolist (clause (reverse (query-state-group-by-clauses query)))
      (push clause clauses))

    ;; Add HAVING clauses
    (dolist (clause (reverse (query-state-having-clauses query)))
      (push clause clauses))

    ;; Add JOIN clauses
    (dolist (clause (reverse (query-state-join-clauses query)))
      (push clause clauses))

    ;; Add LIMIT clause
    (when (query-state-limit-clause query)
      (push (query-state-limit-clause query) clauses))

    ;; Add OFFSET clause
    (when (query-state-offset-clause query)
      (push (query-state-offset-clause query) clauses))

    ;; Add RETURNING clause
    (when (query-state-returning-clause query)
      (push (query-state-returning-clause query) clauses))

    ;; Create the select statement
    (apply #'stmt:make-statement :select (reverse clauses))))

(defun yield-query (query)
  "Generate SQL string and parameters from a query-state object using SxQL infrastructure"
  (check-type query query-state)
  (let ((select-stmt (query-state-to-select-statement query)))
    (multiple-value-bind (sql params)
        (type:with-yield-binds
          (type:yield select-stmt))
      (cons sql params))))


;;
;; v2 Query Builder
;;

(defun select (&rest clauses)
  "Create a query-state with initial clauses.

   This is the main entry point for building queries. You can pass any
   combination of clauses (FROM, WHERE, ORDER BY, etc.) as arguments.

   Examples:
     (select)  ; Empty query: SELECT *
     (select (from :users))  ; Simple query: SELECT * FROM users
     (select (from :users) (where (:= :id 123)))  ; With WHERE clause

   The returned query-state is immutable - use the -> threading macro
   to build upon it:
     (-> (select (from :users))
         (where (:= :active 1))
         (order-by :name))"
  (let ((query (make-query-state)))
    (dolist (clause clauses query)
      (setf query (add-clause query clause)))))

;;
;; v2 Clause Macros (copied from sxql.lisp)
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-op (object)
    (if (and (listp object)
             (keywordp (car object)))
        `(op:make-op ,(car object) ,@(mapcar #'expand-op (cdr object)))
        object))

  (defun expand-expression (expressions)
    (cond
      ((not (listp expressions)) expressions)
      ((and (symbolp (car expressions))
            (not (keywordp (car expressions))))
       expressions)
      (t (mapcar #'expand-op expressions)))))

(defmacro from (&rest statements)
  `(clause:make-clause :from ,@(mapcar #'expand-op statements)))

(defmacro where (expression)
  `(clause:make-clause :where
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

(defmacro fields (&rest fields)
  `(clause:make-clause :fields ,@(mapcar #'expand-op fields)))

(defmacro order-by (&rest expressions)
  `(clause:make-clause :order-by ,@(mapcar #'expand-op expressions)))

(defmacro group-by (&rest expressions)
  `(apply #'clause:make-clause :group-by (list ,@(mapcar #'expand-op expressions))))

(defmacro having (expression)
  `(clause:make-clause :having
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

(defmacro returning (&rest expressions)
  `(apply #'clause:make-clause :returning
          (list ,@(mapcar (lambda (expr)
                            `,(expand-op expr))
                          expressions))))

(defun limit (count1 &optional count2)
  (apply #'clause:make-clause :limit `(,count1 ,@(and count2 (list count2)))))

(defun offset (offset)
  (clause:make-clause :offset offset))

(defmacro join (table &key (kind :inner) on using)
  `(clause:make-clause :join ,(expand-op table)
                :kind ,kind
                ,@(if on
                      `(:on ,(expand-op on))
                      nil)
                ,@(if using
                      `(:using (list ,@(mapcar #'expand-op using)))
                      nil)))

(defmacro inner-join (table &key on using)
  `(join ,table :kind :inner :on ,on :using ,using))

(defmacro left-join (table &key on using)
  `(join ,table :kind :left :on ,on :using ,using))

(defmacro right-join (table &key on using)
  `(join ,table :kind :right :on ,on :using ,using))

(defmacro full-join (table &key on using)
  `(join ,table :kind :full :on ,on :using ,using))

;;
;; v2 Threading Utilities
;;

(defgeneric add-clause (query clause)
  (:method ((query query-state) clause)
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
       (add-returning-clause query clause)))))

(defmacro -> (value &rest forms)
  "Smart threading macro that dispatches based on clause types returned by forms.

   This macro enables immutable query composition by threading clauses through
   query-state objects. Each step creates a new query-state, leaving the original
   unchanged.

   Examples:
     (-> (from :users)
         (where (:= :active 1))
         (order-by :name))

     (let ((base-query (select (from :users))))
       (-> base-query
           (where (:= :department \"engineering\"))
           (limit 10)))"
  (if (null forms)
      value
      (a:with-gensyms (threaded-var)
        `(let* ((,threaded-var ,value)
                (,threaded-var
                  (typecase ,threaded-var
                    (query-state ,threaded-var)
                    (otherwise (add-clause (make-query-state) ,threaded-var)))))
           ,@(loop for form in forms
                   collect `(setf ,threaded-var
                                  (add-clause ,threaded-var
                                              ,(if (listp form)
                                                   ;; For list forms, call the function with its arguments (no threading)
                                                   `(,(car form) ,@(cdr form))
                                                   ;; For symbol forms, call the function with no arguments
                                                   `(,form)))))
           ,threaded-var))))

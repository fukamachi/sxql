(defpackage #:sxql
  (:use #:cl
        #:sxql/statement
        #:sxql/composed-statement
        #:sxql/clause)
  (:shadow #:primary-key
           #:foreign-key
           #:key)
  (:import-from #:sxql/sql-type
                #:sql-clause-list
                #:yield
                #:make-sql-symbol
                #:make-sql-symbol*
                #:*use-placeholder*
                #:*quote-character*)
  (:import-from #:sxql/compile
                #:sql-compile)
  (:import-from #:sxql/operator
                #:make-op
                #:detect-and-convert
                #:union-op
                #:union-all-op
                #:*sql-symbol-conversion*)
  (:import-from #:trivia
                #:match)
  (:export #:yield
           #:sql-compile
           #:add-child
           #:merge-statements
           #:make-statement
           #:make-clause
           #:make-op
           #:make-sql-symbol
           #:make-sql-symbol*
           #:compose-statements
           #:*use-placeholder*
           #:*quote-character*
           #:*sql-symbol-conversion*

           ;; Statement types
           #:select-statement
           #:insert-into-statement
           #:update-statement
           #:delete-from-statement
           #:create-table-statement
           #:drop-table-statement
           #:alter-table-statement
           #:create-index-statement
           #:drop-index-statement
           #:explain-statement
           #:create-view-statement
           #:drop-view-statement
           #:select-statement-designator

           ;; Statement macros/functions
           #:select
           #:insert-into
           #:update
           #:delete-from
           #:create-table
           #:drop-table
           #:alter-table
           #:pragma
           #:union-queries
           #:union-all-queries
           #:create-index
           #:drop-index
           #:explain
           #:create-view
           #:drop-view

           ;; Clause macros/functions
           #:fields
           #:distinct-on
           #:from
           #:where
           #:order-by
           #:group-by
           #:having
           #:returning
           #:for
           #:limit
           #:offset
           #:set=
           #:join
           #:inner-join
           #:left-join
           #:right-join
           #:full-join

           ;; Key and constraint functions
           #:primary-key
           #:unique-key
           #:index-key
           #:foreign-key

           ;; Column modification functions
           #:add-column
           #:modify-column
           #:alter-column
           #:drop-column
           #:change-column
           #:rename-to
           #:add-primary-key
           #:drop-primary-key
           #:drop-constraint

           ;; Conflict resolution
           #:on-duplicate-key-update
           #:on-conflict-do-nothing
           #:on-conflict-do-update))
(in-package #:sxql)

(cl-package-locks:lock-package '#:sxql)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun expand-op (object)
    (if (and (listp object)
             (keywordp (car object)))
        `(make-op ,(car object) ,@(mapcar #'expand-op (cdr object)))
        object))

  (defun expand-expression (expressions)
    (cond
      ((not (listp expressions)) expressions)
      ((and (symbolp (car expressions))
            (not (keywordp (car expressions))))
       expressions)
      (t (mapcar #'expand-op expressions)))))

(defmacro select (fields &body clauses)
  `(make-statement :select
                   ,(match fields
                      ((or (list* (type keyword) _)
                           (list* (list* (type keyword) _) _))
                       `(make-clause :fields
                                     ,@(mapcar #'expand-op fields)))
                      ((type keyword) `(fields ,fields))
                      ((or (type symbol)
                           (list* (type symbol) _))
                       `(convert-if-fields-clause ,fields))
                      (otherwise fields))
                   ,@clauses))

(defmacro insert-into (table &body clauses)
  `(make-statement :insert-into
                   ,(expand-expression table)
                   ,@(if (and (listp (car clauses))
                              (keywordp (caar clauses)))
                         `(',(car clauses) ,@(cdr clauses))
                         clauses)))

(defmacro update (table &body clauses)
  `(make-statement :update
                   ,(expand-expression table) ,@clauses))

(defmacro delete-from (table &body clauses)
  `(make-statement :delete-from
                   ,(expand-expression table) ,@clauses))

(defmacro create-table (table column-definitions &body options)
  `(make-statement :create-table
                   ',(expand-expression table)
                   ,(if (listp (car column-definitions))
                        `(list ,@(if column-definitions
                                     (mapcar
                                      (lambda (column)
                                        `(make-column-definition-clause ',(car column) ,@(cdr column)))
                                      column-definitions)
                                     nil))
                        `,column-definitions)
                   ,@(if (and (null (cdr options))
                              (null (car options)))
                         nil
                         options)))

(defmacro drop-table (table &key if-exists)
  `(make-statement :drop-table
                   ,(expand-expression table) :if-exists ,if-exists))

(defmacro alter-table (table &body clauses)
  `(make-statement :alter-table
                   ,(expand-expression table) ,@clauses))

(defun pragma (name &optional value)
  (make-statement :pragma name value))

(defun union-queries (&rest queries)
  (apply #'make-op :union queries))

(defun union-all-queries (&rest queries)
  (apply #'make-op :union-all queries))

(defun create-index (index-name &rest args &key unique using on if-not-exists)
  (declare (ignore unique using on if-not-exists))
  (apply #'make-statement :create-index index-name
         args))

(defun drop-index (index-name &key if-exists on)
  (make-statement :drop-index index-name :if-exists if-exists :on on))

(defun explain (statement &key analyze verbose)
  (make-statement :explain statement :analyze analyze :verbose verbose))

;;
;; Clauses

(defmacro fields (&rest fields)
  `(make-clause :fields ,@(mapcar #'expand-op fields)))

(defmacro distinct-on (columns &rest fields)
  `(make-clause :distinct-on (list ,@columns) ,@(mapcar #'expand-op fields)))

(defun convert-if-fields-clause (clause)
  (match clause
    ((or (list* (type keyword) _)
         (list* (list* (type keyword) _) _))
     (apply #'make-clause :fields clause))
    ((type keyword) (fields clause))
    (otherwise clause)))

(defmacro from (&rest statements)
  `(make-clause :from ,@(mapcar #'expand-op statements)))

(defmacro where (expression)
  `(make-clause :where
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

(defmacro order-by (&rest expressions)
  `(make-clause :order-by ,@(expand-expression expressions)))

(defmacro group-by (&rest expressions)
  `(apply #'make-clause :group-by ',expressions))

(defmacro having (expression)
  `(make-clause :having
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

(defmacro returning (&rest expressions)
  `(apply #'make-clause :returning
          (list ,@(mapcar (lambda (expr)
                            (if (and (listp expr)
                                     (keywordp (car expr)))
                              (expand-op expr)
                              `,expr))
                          expressions))))

(defmacro for (update-type &key of nowait skip-locked)
  (let ((ident-list (if (keywordp of)
                        `(list ,of)
                        `(quote ,of))))
    `(make-clause :updatability ,update-type :of ,ident-list :nowait ,nowait :skip-locked ,skip-locked)))

(defun limit (count1 &optional count2)
  (apply #'make-clause :limit `(,count1 ,@(and count2 (list count2)))))

(defun offset (offset)
  (make-clause :offset offset))

(defmacro set= (&rest args)
  `(make-clause :set= ,@(mapcar #'expand-op args)))

(defmacro join (table &key (kind :inner) on using)
  `(make-clause :join ,(expand-op table)
                :kind ,kind
                ,@(if on
                      `(:on ,(if (and (listp on)
                                      (keywordp (car on)))
                                 (expand-op on)
                                 `,on))
                      nil)
                ,@(if using
                      `(:using ',using)
                      nil)))

(defmacro inner-join (table &key on using)
  `(join ,table :kind :inner :on ,on :using ,using))

(defmacro left-join (table &key on using)
  `(join ,table :kind :left :on ,on :using ,using))

(defmacro right-join (table &key on using)
  `(join ,table :kind :right :on ,on :using ,using))

(defmacro full-join (table &key on using)
  `(join ,table :kind :full :on ,on :using ,using))

(defun key-clause-expand (type key-args)
  (if (cdr key-args)
      `(make-clause ,type
                    ,(car key-args)
                    ',(cadr key-args))
      `(make-clause ,type
                    ',(car key-args))))

(defun primary-key (&rest key-args)
  (apply #'make-clause :primary-key key-args))

(defun unique-key (&rest key-args)
  (apply #'make-clause :unique-key key-args))

(defun index-key (&rest key-args)
  (apply #'make-clause :key key-args))

(defun foreign-key (column-names &key references on-delete on-update)
  (flet ((canonicalize-action (action)
           (etypecase action
             (keyword
              (ecase action
                (:no-action "NO ACTION")
                (:set-null "SET NULL")
                ((:restrict :cascade)
                 (symbol-name action))))
             (string action)
             (null action))))
    (make-clause :foreign-key
                 column-names
                 :references references
                 :on-delete (canonicalize-action on-delete)
                 :on-update (canonicalize-action on-update))))

(defun add-column (column-name &rest args)
  (apply #'make-clause :add-column column-name args))

(defun modify-column (column-name &rest args)
  (apply #'make-clause :modify-column column-name args))

(defun alter-column (column-name &rest args)
  (apply #'make-clause :alter-column column-name args))

(defun drop-column (column-name)
  (make-clause :drop-column column-name))

(defun change-column (old-column-name new-column-name &rest args)
  (apply #'make-clause
         :change-column
         old-column-name new-column-name
         args))

(defun rename-to (new-table-name)
  (make-clause :rename-to new-table-name))

(defun add-primary-key (&rest column-names)
  (apply #'make-clause :add-primary-key column-names))

(defun drop-primary-key ()
  (make-clause :drop-primary-key))

(defun drop-constraint (constraint-name)
  (make-clause :drop-constraint constraint-name))

(defmacro on-duplicate-key-update (&rest args)
  `(make-clause :on-duplicate-key-update ,@(mapcar #'expand-op args)))

(defmacro on-conflict-do-nothing (&optional (conflict-target nil))
  `(make-clause :on-conflict-do-nothing ,conflict-target))

(defmacro on-conflict-do-update (conflict-target update-set &optional where-condition)
  `(make-clause :on-conflict-do-update
                ,conflict-target
                ,update-set
                ,where-condition))

(defmacro create-view (view-name &key or-replace as)
  `(make-statement :create-view ,view-name
                   :or-replace ,or-replace
                   :as ,as))

(defmacro drop-view (view-name)
  `(make-statement :drop-view ,view-name))


;;
;; Types

(deftype select-statement-designator ()
  '(or
    select-statement
    composed-statement
    union-op
    union-all-op))

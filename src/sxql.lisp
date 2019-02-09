(in-package :cl-user)
(defpackage sxql
  (:use :cl
        :sxql.statement
        :sxql.composed-statement
        :sxql.clause
        :optima)
  (:shadow :primary-key
           :foreign-key
           :key)
  (:import-from :sxql.sql-type
                :sql-clause-list
                :yield
                :make-sql-symbol
                :*use-placeholder*
                :*quote-character*)
  (:import-from :sxql.compile
                :sql-compile)
  (:import-from :sxql.operator
                :make-op
                :detect-and-convert
                :union-op
                :union-all-op
                :*sql-symbol-conversion*)
  (:export :yield
           :sql-compile
           :add-child
           :merge-statements
           :make-statement
           :make-clause
           :make-op
           :make-sql-symbol
           :compose-statements
           :*use-placeholder*
           :*quote-character*
           :*sql-symbol-conversion*

           :select-statement
           :insert-into-statement
           :update-statement
           :delete-from-statement
           :create-table-statement
           :drop-table-statement
           :alter-table-statement
           :create-index-statement
           :drop-index-statement))
(in-package :sxql)

(cl-syntax:use-syntax :annot)

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

@export
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

@export
(defmacro insert-into (table &body clauses)
  `(make-statement :insert-into
                   ,(expand-expression table)
                   ,@(if (and (listp (car clauses))
                              (keywordp (caar clauses)))
                         `(',(car clauses) ,@(cdr clauses))
                         clauses)))

@export
(defmacro update (table &body clauses)
  `(make-statement :update
                   ,(expand-expression table) ,@clauses))

@export
(defmacro delete-from (table &body clauses)
  `(make-statement :delete-from
                   ,(expand-expression table) ,@clauses))

@export
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

@export
(defmacro drop-table (table &key if-exists)
  `(make-statement :drop-table
                   ,(expand-expression table) :if-exists ,if-exists))

@export
(defmacro alter-table (table &body clauses)
  `(make-statement :alter-table
                   ,(expand-expression table) ,@clauses))

@export
(defun pragma (name &optional value)
  (make-statement :pragma name value))

@export
(defun union-queries (&rest queries)
  (apply #'make-op :union queries))

@export
(defun union-all-queries (&rest queries)
  (apply #'make-op :union-all queries))

@export
(defun create-index (index-name &rest args &key unique using on if-not-exists)
  (declare (ignore unique using on if-not-exists))
  (apply #'make-statement :create-index index-name
         args))

@export
(defun drop-index (index-name &key if-exists on)
  (make-statement :drop-index index-name :if-exists if-exists :on on))

;;
;; Clauses

@export
(defmacro fields (&rest fields)
  `(make-clause :fields ,@(mapcar #'expand-op fields)))

(defun convert-if-fields-clause (clause)
  (match clause
    ((or (list* (type keyword) _)
         (list* (list* (type keyword) _) _))
     (apply #'make-clause :fields clause))
    ((type keyword) (fields clause))
    (otherwise clause)))

@export
(defmacro from (&rest statements)
  `(make-clause :from ,@(mapcar #'expand-op statements)))

@export
(defmacro where (expression)
  `(make-clause :where
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

@export
(defmacro order-by (&rest expressions)
  `(make-clause :order-by ,@(expand-expression expressions)))

@export
(defmacro group-by (&rest expressions)
  `(apply #'make-clause :group-by ',expressions))

@export
(defmacro having (expression)
  `(make-clause :having
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

@export
(defmacro returning (expression)
  `(make-clause :returning
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

@export
(defmacro for (update-type &key of nowait)
  (let ((ident-list (if (keywordp of)
                        `(list ,of)
                        `(quote ,of))))
    `(make-clause :updatability ,update-type :of ,ident-list :nowait ,nowait)))

@export
(defun limit (count1 &optional count2)
  (apply #'make-clause :limit `(,count1 ,@(and count2 (list count2)))))

@export
(defun offset (offset)
  (make-clause :offset offset))

@export
(defmacro set= (&rest args)
  `(make-clause :set= ,@(mapcar #'expand-op args)))

@export
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

@export
(defmacro inner-join (table &key on using)
  `(join ,table :kind :inner :on ,on :using ,using))

@export
(defmacro left-join (table &key on using)
  `(join ,table :kind :left :on ,on :using ,using))

@export
(defmacro right-join (table &key on using)
  `(join ,table :kind :right :on ,on :using ,using))

@export
(defmacro full-join (table &key on using)
  `(join ,table :kind :full :on ,on :using ,using))

(defun key-clause-expand (type key-args)
  (if (cdr key-args)
      `(make-clause ,type
                    ,(car key-args)
                    ',(cadr key-args))
      `(make-clause ,type
                    ',(car key-args))))

@export
(defun primary-key (&rest key-args)
  (apply #'make-clause :primary-key key-args))

@export
(defun unique-key (&rest key-args)
  (apply #'make-clause :unique-key key-args))

@export
(defun index-key (&rest key-args)
  (apply #'make-clause :key key-args))

@export
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

@export
(defun add-column (column-name &rest args)
  (apply #'make-clause :add-column column-name args))

@export
(defun modify-column (column-name &rest args)
  (apply #'make-clause :modify-column column-name args))

@export
(defun alter-column (column-name &rest args)
  (apply #'make-clause :alter-column column-name args))

@export
(defun drop-column (column-name)
  (make-clause :drop-column column-name))

@export
(defun change-column (old-column-name new-column-name &rest args)
  (apply #'make-clause
         :change-column
         old-column-name new-column-name
         args))

@export
(defun rename-to (new-table-name)
  (make-clause :rename-to new-table-name))

@export
(defun add-primary-key (&rest column-names)
  (apply #'make-clause :add-primary-key column-names))

@export
(defun drop-primary-key ()
  (make-clause :drop-primary-key))

@export
(defmacro on-duplicate-key-update (&rest args)
  `(make-clause :on-duplicate-key-update ,@(mapcar #'expand-op args)))

@export
(defmacro on-conflict-do-nothing (&optional (conflict-target nil))
  `(make-clause :on-conflict-do-nothing ,conflict-target))

@export
(defmacro on-conflict-do-update (conflict-target update-set &optional where-condition)
  `(make-clause :on-conflict-do-update
                ,conflict-target
                ,update-set
                ,where-condition))


;;
;; Types

@export
(deftype select-statement-designator ()
  '(or
    select-statement
    composed-statement
    union-op
    union-all-op))

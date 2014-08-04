#|
  This file is a part of sxql project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

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
                :*use-placeholder*
                :*quote-character*)
  (:import-from :sxql.compile
                :sql-compile)
  (:import-from :sxql.operator
                :make-op
                :detect-and-convert
                :union-op
                :union-all-op)
  (:export :yield
           :sql-compile
           :add-child
           :make-statement
           :make-clause
           :make-op
           :compose-statements
           :*use-placeholder*
           :*quote-character*

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
(defun union-queries (&rest queries)
  (apply #'make-op :union queries))

@export
(defun union-all-queries (&rest queries)
  (apply #'make-op :union-all queries))

@export
(defun create-index (index-name &rest args &key unique using on)
  (declare (ignore unique using on))
  (apply #'make-statement :create-index index-name
         args))

@export
(defun drop-index (index-name &key if-exists on)
  (make-statement :drop-index index-name :if-exists if-exists :on on))

;;
;; Clauses

@export
(defmacro fields (&rest fields)
  `(make-clause :fields ,@(mapcar (lambda (field)
                                    (expand-op field)) fields)))

(defun convert-if-fields-clause (clause)
  (match clause
    ((or (list* (type keyword) _)
         (list* (list* (type keyword) _) _))
     (apply #'make-clause :fields clause))
    ((type keyword) (fields clause))
    (otherwise clause)))

@export
(defmacro from (statement)
  `(make-clause :from ,(expand-op statement)))

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
(defun limit (count1 &optional count2)
  (apply #'make-clause :limit `(,count1 ,@(and count2 (list count2)))))

@export
(defun offset (offset)
  (make-clause :offset offset))

@export
(defun set= (&rest args)
  (apply #'make-clause :set= args))

@export
(defmacro join (table &key (kind :inner) on using)
  `(make-clause :join ,(expand-op table)
                :kind ,kind
                ,@(if on
                      `(:on (make-op ,@on))
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
(defun foreign-key (column-names &key references)
  (make-clause :foreign-key
               column-names
               :references
               references))

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


;;
;; Types

@export
(deftype select-statement-designator ()
  '(or
    select-statement
    composed-statement
    union-op
    union-all-op))

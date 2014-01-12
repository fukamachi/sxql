#|
  This file is a part of sxql project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql
  (:use :cl
        :sxql.statement
        :sxql.clause)
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
                :detect-and-convert)
  (:export :yield
           :sql-compile
           :add-child
           :make-statement
           :make-clause
           :make-op
           :*use-placeholder*
           :*quote-character*))
(in-package :sxql)

(cl-syntax:use-syntax :annot)

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
    (t (mapcar #'expand-op expressions))))

@export
(defmacro select (field &body clauses)
  (let ((clauses-g (gensym "CLAUSES")))
    `(let ((,clauses-g (list ,@clauses)))
       (apply #'make-statement :select ,(if (listp field)
                                            (if (and (symbolp (car field))
                                                     (not (keywordp (car field))))
                                                field
                                                `(list ,@(mapcar #'expand-op field)))
                                            `,field) ,clauses-g))))

@export
(defmacro insert-into (table &body clauses)
  (let ((clauses-g (gensym "CLAUSES")))
    `(let ((,clauses-g (list ,@clauses)))
       (apply #'make-statement :insert-into
              ,(expand-expression table)
              ,clauses-g))))

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
                   ,(expand-expression table)
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
(defun drop-index (index-name &key if-exists)
  (make-statement :drop-index index-name :if-exists if-exists))

;;
;; Clauses

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
(defmacro left-join (table &key on using)
  `(make-clause :left-join ,(expand-op table)
                ,@(if on
                      `(:on (make-op ,@on))
                      nil)
                ,@(if using
                      `(:using ',using)
                      nil)))

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
(defun drop-column (column-name)
  (make-clause :drop-column column-name))

@export
(defun change-column (old-column-name new-column-name &rest args)
  (apply #'make-clause
         :change-column
         old-column-name new-column-name
         args))

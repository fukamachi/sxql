#|
  This file is a part of sxql project.
  Copyright (c) 2013 Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sxql
  (:use :cl
        :sxql.statement
        :sxql.clause)
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
           :*use-placeholder*
           :*quote-character*))
(in-package :sxql)

(cl-syntax:use-syntax :annot)

(defun expand-op (object)
  (if (and (listp object)
           (keywordp (car object)))
      `(make-op ,(car object) ,@(mapcar #'expand-op (cdr object)))
      object))

@export
(defmacro select (field &rest clauses)
  (let ((clauses-g (gensym "CLAUSES")))
    `(let ((,clauses-g (list ,@clauses)))
       (check-type ,clauses-g sql-clause-list)
       (apply #'make-statement :select ,(if (listp field)
                                            `(list ,@(mapcar #'expand-op field))
                                            `,field) ,clauses-g))))

@export
(defmacro insert-into (table &rest clauses)
  (let ((clauses-g (gensym "CLAUSES")))
    `(let ((,clauses-g (list ,@clauses)))
       (check-type ,clauses-g sql-clause-list)
       (apply #'make-statement :insert-into
              ',table ,clauses-g))))

@export
(defmacro update (table &rest clauses)
  `(make-statement :update
                   ',table ,@clauses))

@export
(defmacro delete-from (table &rest clauses)
  `(make-statement :delete-from
                   ',table ,@clauses))

@export
(defmacro create-table (table column-definitions &rest options)
  `(apply #'make-statement :create-table
          ',table ',column-definitions ',options))

@export
(defmacro drop-table (table &key if-exists)
  `(make-statement :drop-table
                   ',table :if-exists ,if-exists))

@export
(defun union-queries (&rest queries)
  (apply #'sxql.operator:make-op :union queries))

@export
(defun union-all-queries (&rest queries)
  (apply #'sxql.operator:make-op :union-all queries))

;;
;; Clauses

@export
(defmacro from (statement)
  `(make-clause :from
                ,(if (and (listp statement)
                          (keywordp (car statement)))
                     `(make-op ,@statement)
                     statement)))

@export
(defmacro where (expression)
  `(make-clause :where
                ,(if (and (listp expression)
                          (keywordp (car expression)))
                     (expand-op expression)
                     `,expression)))

@export
(defmacro order-by (&rest expressions)
  `(apply #'make-clause :order-by ',expressions))

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
(defmacro set= (&rest args)
  `(make-clause :set= ,@args))

@export
(defmacro left-join (table &key on)
  `(make-left-join-clause (detect-and-convert ',table)
                          :on (detect-and-convert ',on)))

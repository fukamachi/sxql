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
                :*use-placeholder*)
  (:import-from :sxql.operator
                :detect-and-convert)
  (:export :yield
           :*use-placeholder*))
(in-package :sxql)

(cl-syntax:use-syntax :annot)

@export
(defun select (field &rest args)
  (check-type args sql-clause-list)
  (apply #'make-statement :select
         (make-clause :field field)
         args))

;;
;; Clauses

@export
(defun from (statement)
  (make-clause :from statement))

@export
(defun where (expression)
  (make-clause :where expression))

@export
(defun order-by (expression)
  (make-clause :order-by expression))

@export
(defun group-by (expression)
  (make-clause :group-by expression))

@export
(defun limit (count1 &optional count2)
  (apply #'make-clause :limit `(,count1 ,@(and count2 (list count2)))))

@export
(defun offset (offset)
  (make-clause :offset offset))


@export
(defun left-join (table &key on)
  (make-left-join-clause (detect-and-convert table)
                         :on (detect-and-convert on)))

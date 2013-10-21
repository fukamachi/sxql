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
  (:import-from :sxql.operator
                :detect-and-convert)
  (:export :yield
           :*use-placeholder*
           :*quote-character*))
(in-package :sxql)

(cl-syntax:use-syntax :annot)

@export
(defun select (field &rest clauses)
  (check-type clauses sql-clause-list)
  (apply #'make-statement :select
         field clauses))

@export
(defun insert-into (table &rest clauses)
  (check-type clauses sql-clause-list)
  (apply #'make-statement :insert-into
         table clauses))

@export
(defun update (table &rest clauses)
  (apply #'make-statement :update
         table clauses))

@export
(defun delete-from (table &rest clauses)
  (apply #'make-statement :delete-from
         table clauses))

;;
;; Clauses

@export
(defun from (statement)
  (make-clause :from statement))

@export
(defun where (expression)
  (make-clause :where expression))

@export
(defun order-by (&rest expressions)
  (apply #'make-clause :order-by expressions))

@export
(defun group-by (&rest expressions)
  (apply #'make-clause :group-by expressions))

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
(defun left-join (table &key on)
  (make-left-join-clause (detect-and-convert table)
                         :on (detect-and-convert on)))

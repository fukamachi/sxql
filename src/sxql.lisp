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
                :stringify))
(in-package :sxql)

(cl-syntax:use-syntax :annot)

@export
(defun select (&key field from where order-by group-by limit offset)
  (stringify
   (make-statement :select
                   :field (and field
                               (make-clause :field field))
                   :from (and from
                              (make-clause :from from))
                   :where (and where
                               (make-clause :where where))
                   :order-by (and order-by
                                  (make-clause :order-by order-by))
                   :group-by (and group-by
                                  (make-clause :group-by group-by))
                   :limit (and limit
                               (apply #'make-clause :limit (if (listp limit)
                                                               limit
                                                               (list limit))))
                   :offset (and offset
                                (make-clause :offset offset)))))





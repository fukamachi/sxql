(in-package :cl-user)
(defpackage sxql.clause
  (:use :cl
        :sxql.sql-type
        :sxql.operator))
(in-package :sxql.clause)

(cl-syntax:use-syntax :annot)

@export
(defstruct (field-clause (:include sql-clause (name ""))
                         (:constructor make-field-clause (fields)))
  (fields nil :type sql-expression))

@export
(defstruct (from-clause (:include statement-clause (name "FROM"))
                        (:constructor make-from-clause (statement))))

@export
(defstruct (where-clause (:include expression-clause (name "WHERE"))
                         (:constructor make-where-clause (expression))))

@export
(defstruct (order-by-clause (:include expression-clause (name "ORDER BY"))
                            (:constructor make-order-by-clause (expression))))

@export
(defstruct (limit-clause (:include sql-clause (name "LIMIT"))
                         (:constructor make-limit-clause (count1 &optional count2)))
  (count1 nil :type sql-variable)
  (count2 nil :type (or null sql-variable)))

@export
(defstruct (offset-clause (:include sql-clause (name "OFFSET"))
                          (:constructor make-offset-clause (offset)))
  (offset nil :type sql-variable))

@export
(defstruct (group-by-clause (:include expression-clause (name "GROUP BY"))
                            (:constructor make-group-by-clause (expression))))

(defun find-make-clause (clause-name &optional (package *package*))
  (find-constructor clause-name #.(string :-clause)
                    :package package))

@export
(defun make-clause (clause-name &rest args)
  (apply (find-make-clause clause-name #.*package*)
         (mapcar #'detect-and-convert args)))

(defmethod stringify ((clause field-clause))
  (if (field-clause-fields clause)
      (stringify (field-clause-fields clause))
      (values "*" nil)))

(defmethod stringify ((clause limit-clause))
  (let ((*use-placeholder* nil))
    (values
     (format nil "LIMIT ~A~:[~;~:*, ~A~]"
             (stringify (limit-clause-count1 clause))
             (if (limit-clause-count2 clause)
                 (stringify (limit-clause-count2 clause))
                 nil))
     nil)))

(defmethod stringify ((clause offset-clause))
  (let ((*use-placeholder* nil))
    (values
     (format nil "OFFSET ~A"
             (stringify (offset-clause-offset clause)))
     nil)))

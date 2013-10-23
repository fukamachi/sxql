(in-package :cl-user)
(defpackage sxql.statement
  (:use :cl
        :sxql.sql-type)
  (:import-from :sxql.operator
                :find-constructor
                :detect-and-convert)
  (:import-from :sxql.clause
                :make-column-definition-clause))
(in-package :sxql.statement)

(cl-syntax:use-syntax :annot)

(defstruct (select-statement (:include sql-composed-statement (name "SELECT"))
                             (:constructor make-select-statement
                                 (fields &rest statements
                                  &aux (children
                                        (list* (if (listp fields)
                                                   (apply #'make-sql-list fields)
                                                   fields)
                                               statements))))))

(defstruct (insert-into-statement (:include sql-composed-statement (name "INSERT INTO"))
                                  (:constructor make-insert-into-statement (&rest children))))

(defstruct (update-statement (:include sql-composed-statement (name "UPDATE"))
                             (:constructor make-update-statement (&rest children))))

(defstruct (delete-from-statement (:include sql-composed-statement (name "DELETE FROM"))
                                  (:constructor make-delete-from-statement (&rest children))))

(defstruct (create-table-statement (:include sql-composed-statement (name "CREATE TABLE"))
                                   (:constructor make-create-table-statement (&rest children))))

(defstruct (drop-table-statement (:include sql-statement (name "DROP TABLE"))
                                 (:constructor make-drop-table-statement (table &key if-exists)))
  (table nil :type sql-symbol)
  (if-exists nil :type boolean))

(defun find-make-statement (statement-name &optional (package *package*))
  (find-constructor statement-name #.(string :-statement)
                    :package package))

@export
(defmethod make-statement (statement-name &rest args)
  (apply (find-make-statement statement-name #.*package*)
         (mapcar #'detect-and-convert args)))

(defmethod make-statement ((statement-name (eql :create-table)) &rest args)
  (destructuring-bind (table column-definitions &rest options) args
    (apply #'make-create-table-statement
           (detect-and-convert table)
           (apply #'make-sql-list
                  (mapcar #'(lambda (column)
                              (apply #'make-column-definition-clause
                                     (detect-and-convert (car column))
                                     (loop for (key val) on (cdr column) by #'cddr
                                           if (and (eq key :type) (symbolp val))
                                             append (list key (make-sql-keyword (string-upcase val)))
                                           else
                                             append (list key (detect-and-convert val)))))
                          column-definitions))
           options)))

(defmethod make-statement ((statement-name (eql :drop-table)) &rest args)
  (destructuring-bind (table &key if-exists) args
    (make-drop-table-statement (detect-and-convert table)
                               :if-exists if-exists)))

(defmethod yield ((statement drop-table-statement))
  (values
   (format nil "DROP TABLE~:[~; IF EXISTS~] ~A"
           (drop-table-statement-if-exists statement)
           (yield (drop-table-statement-table statement)))
   nil))

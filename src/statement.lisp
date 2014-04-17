(in-package :cl-user)
(defpackage sxql.statement
  (:use :cl
        :sxql.sql-type)
  (:import-from :sxql.operator
                :find-constructor
                :detect-and-convert)
  (:import-from :sxql.clause
                :column-definition-clause
                :make-column-definition-clause
                :*inside-insert-into*))
(in-package :sxql.statement)

(cl-syntax:use-syntax :annot)

@export
(defmethod add-child ((statement sql-composed-statement) child)
  (rplacd (last (sql-composed-statement-children statement))
          (list child))
  statement)

(defstruct (select-statement (:include sql-composed-statement (name "SELECT"))
                             (:constructor make-select-statement (&rest children))))

(defstruct (insert-into-statement (:include sql-composed-statement (name "INSERT INTO"))
                                  (:constructor make-insert-into-statement (&rest children))))

(defstruct (update-statement (:include sql-composed-statement (name "UPDATE"))
                             (:constructor make-update-statement (&rest children))))

(defstruct (delete-from-statement (:include sql-composed-statement (name "DELETE FROM"))
                                  (:constructor make-delete-from-statement (&rest children))))

(defstruct (create-table-statement (:include sql-composed-statement (name "CREATE TABLE"))
                                   (:constructor make-create-table-statement (table &key if-not-exists children
                                                                              &aux (children (cons table children)))))
  (if-not-exists nil :type boolean))

(defstruct (drop-table-statement (:include sql-statement (name "DROP TABLE"))
                                 (:constructor make-drop-table-statement (table &key if-exists)))
  (table nil :type sql-symbol)
  (if-exists nil :type boolean))

(defstruct (alter-table-statement (:include sql-statement (name "ALTER TABLE"))
                                  (:constructor make-alter-table-statement (table &rest children
                                                                            &aux (children
                                                                                  (apply #'make-sql-splicing-list children)))))
  (table nil :type sql-symbol)
  (children nil))

(defstruct (create-index-statement (:include sql-statement (name "CREATE INDEX"))
                                   (:constructor make-create-index-statement (index-name table-name columns &key unique using)))
  (index-name nil :type sql-symbol)
  (table-name nil :type sql-symbol)
  (columns nil :type sql-list)
  (unique nil :type boolean)
  (using nil :type (or null sql-keyword)))

(defstruct (drop-index-statement (:include sql-statement (name "DROP INDEX"))
                                 (:constructor make-drop-index-statement (index-name &key if-exists on)))
  (index-name nil :type sql-symbol)
  (if-exists nil :type boolean)
  (on nil :type (or null sql-symbol)))

(defun find-make-statement (statement-name &optional (package *package*))
  (find-constructor statement-name #.(string :-statement)
                    :package package))

@export
(defgeneric make-statement (statement-name &rest args))

@export
(defmethod make-statement (statement-name &rest args)
  (apply (find-make-statement statement-name #.*package*)
         (remove nil (mapcar #'detect-and-convert args))))

(defmethod make-statement ((statement-name (eql :insert-into)) &rest args)
  (destructuring-bind (table-name &rest restargs) args
    (apply #'make-insert-into-statement
           (if (listp (car restargs))
               (list*
                (detect-and-convert table-name)
                (apply #'make-sql-list
                       (mapcar #'detect-and-convert (car restargs)))
                (mapcar #'detect-and-convert
                        (cdr restargs)))
               (mapcar #'detect-and-convert args)))))

(defmethod make-statement ((statement-name (eql :create-table)) &rest args)
  (destructuring-bind (table-and-args column-definitions &rest options) args
    (let ((table-and-args (if (listp table-and-args)
                              table-and-args
                              (list table-and-args))))
      (make-create-table-statement
       (detect-and-convert (car table-and-args))
       :if-not-exists (getf (cdr table-and-args) :if-not-exists)
       :children
       (list (apply #'make-sql-list
                    (append
                     (mapcar #'(lambda (column)
                                 (if (typep column 'column-definition-clause)
                                     column
                                     (apply #'make-column-definition-clause column)))
                             column-definitions)
                     options)))))))

(defmethod make-statement ((statement-name (eql :drop-table)) &rest args)
  (destructuring-bind (table &key if-exists) args
    (make-drop-table-statement (detect-and-convert table)
                               :if-exists if-exists)))

(defmethod make-statement ((statement-name (eql :create-index)) &rest args)
  (destructuring-bind (index-name  &key unique using on) args
    (make-create-index-statement (make-sql-symbol index-name)
                                 (detect-and-convert (car on))
                                 (apply #'make-sql-list
                                        (mapcar #'detect-and-convert (cdr on)))
                                 :unique unique
                                 :using (and using
                                             (make-sql-keyword (string using))))))

(defmethod make-statement ((statement-name (eql :drop-index)) &rest args)
  (destructuring-bind (index-name &key if-exists on) args
    (make-drop-index-statement (make-sql-symbol index-name)
                               :if-exists if-exists
                               :on (detect-and-convert on))))

(defmethod yield ((statement create-table-statement))
  (with-yield-binds
    (format nil "~A~:[~; IF NOT EXISTS~] ~{~A~^ ~}"
            (sql-statement-name statement)
            (create-table-statement-if-not-exists statement)
            (mapcar #'yield (sql-composed-statement-children statement)))))

(defmethod yield ((statement drop-table-statement))
  (values
   (format nil "DROP TABLE~:[~; IF EXISTS~] ~A"
           (drop-table-statement-if-exists statement)
           (yield (drop-table-statement-table statement)))
   nil))

(defmethod yield ((statement alter-table-statement))
  (values
   (format nil "ALTER TABLE ~A ~A"
           (yield (alter-table-statement-table statement))
           (yield (alter-table-statement-children statement)))
   nil))

(defmethod yield ((statement insert-into-statement))
  (let ((*inside-insert-into* t))
    (call-next-method)))

(defmethod yield ((statement create-index-statement))
  (values
   (format nil "CREATE~:[~; UNIQUE~] INDEX ~A~:[~; USING ~:*~A~] ON ~A ~A"
           (create-index-statement-unique statement)
           (yield (create-index-statement-index-name statement))
           (and (create-index-statement-using statement)
                (yield (create-index-statement-using statement)))
           (yield (create-index-statement-table-name statement))
           (yield (create-index-statement-columns statement)))
   nil))

(defmethod yield ((statement drop-index-statement))
  (values
   (format nil "DROP INDEX~:[~; IF EXISTS~] ~A~:[~;~:* ON ~A~]"
           (drop-index-statement-if-exists statement)
           (yield (drop-index-statement-index-name statement))
           (and (drop-index-statement-on statement)
                (yield (drop-index-statement-on statement))))
   nil))

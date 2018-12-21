(in-package :cl-user)
(defpackage sxql.statement
  (:use :cl
        :annot.class
        :sxql.sql-type
        :iterate)
  (:import-from :sxql.operator
                :*inside-select*
                :find-constructor
                :detect-and-convert)
  (:import-from :sxql.clause
                :column-definition-clause
                :make-column-definition-clause
                :*inside-insert-into*
                :fields-clause
                :from-clause
                :from-clause-table-name
                :join-clause
                :where-clause
                :compose-where-clauses
                :group-by-clause
                :having-clause
                :returning-clause
                :updatability-clause
                :order-by-clause
                :limit-clause
                :offset-clause)
  (:import-from :sxql.util
                :group-by
                :subdivide)
  (:import-from :alexandria
                :compose
                :when-let))
(in-package :sxql.statement)

(cl-syntax:use-syntax :annot)

@export
(defgeneric add-child (statement child))

(defmethod add-child ((statement sql-composed-statement) child)
  (let ((slot-name (type-of child)))
    (setf (slot-value statement slot-name)
          (nconc (slot-value statement slot-name) (list child))))
  statement)

(defparameter *clause-priority*
  (let ((hash (make-hash-table :test 'eq)))
    (iter
      (for i from 0)
      (for clause in '(fields-clause
                       from-clause
                       join-clause
                       where-clause
                       group-by-clause
                       having-clause
                       returning-clause
                       order-by-clause
                       limit-clause
                       offset-clause
                       updatability-clause))
      (setf (gethash clause hash) i))
    hash))

@export
(defun sort-clause-types (types)
  (sort types
        (lambda (a b)
          (and a b
               (< a b)))
        :key (lambda (type)
               (gethash type *clause-priority*))))

@export
@export-accessors
(defstruct (select-statement (:include sql-composed-statement (name "SELECT"))
                             (:constructor make-select-statement (&rest
                                                                    clauses
                                                                  &key
                                                                    fields-clause
                                                                    from-clause
                                                                    join-clause
                                                                    where-clause
                                                                    group-by-clause
                                                                    having-clause
                                                                    returning-clause
                                                                    order-by-clause
                                                                    limit-clause
                                                                    offset-clause
                                                                    updatability-clause
                                                                  &aux
                                                                    (clause-order
                                                                     (sort-clause-types
                                                                      (delete-duplicates
                                                                       (iter (for (type clause) on clauses by #'cddr)
                                                                         (collect (type-of (car clause))))
                                                                       :from-end t
                                                                       :test #'eq))))))
  clause-order

  (fields-clause nil)
  (from-clause nil)
  (join-clause nil)
  (where-clause nil)
  (group-by-clause nil)
  (having-clause nil)
  (returning-clause nil)
  (order-by-clause nil)
  (limit-clause nil)
  (offset-clause nil)
  (updatability-clause nil))

@export
(defun compute-select-statement-children (select-statement)
  (iter (for (type . score)
             in (sort
                 (iter (for type in '(fields-clause
                                      from-clause
                                      join-clause
                                      where-clause
                                      group-by-clause
                                      having-clause
                                      returning-clause
                                      order-by-clause
                                      limit-clause
                                      offset-clause
                                      updatability-clause))
                   (collect (cons type
                                  (or (position type (select-statement-clause-order select-statement)
                                                :test #'eq)
                                      100))))
                 #'<
                 :key #'cdr))
    (appending
     (let ((clauses (slot-value select-statement type)))
       (if (and (eq type 'where-clause) clauses)
           (list (compose-where-clauses clauses))
           clauses)))))

(defmethod yield ((statement select-statement))
  (let ((*inside-select* t))
    (call-next-method)))

(defmethod yield :before ((statement select-statement))
  (setf (select-statement-children statement)
        (compute-select-statement-children statement)))

@export
(defun select-statement-table-name (select)
  (when-let ((from (select-statement-from-clause select)))
    (from-clause-table-name (car from))))

@export
(defstruct (insert-into-statement (:include sql-composed-statement (name "INSERT INTO"))
                                  (:constructor make-insert-into-statement (&rest children))))

@export
(defstruct (update-statement (:include sql-composed-statement (name "UPDATE"))
                             (:constructor make-update-statement (&rest children))))

@export
(defstruct (delete-from-statement (:include sql-composed-statement (name "DELETE FROM"))
                                  (:constructor make-delete-from-statement (&rest children))))

@export
(defstruct (create-table-statement (:include sql-composed-statement (name "CREATE TABLE"))
                                   (:constructor make-create-table-statement (table &key if-not-exists children)))
  table
  (if-not-exists nil :type boolean))

@export
(defstruct (drop-table-statement (:include sql-statement (name "DROP TABLE"))
                                 (:constructor make-drop-table-statement (table &key if-exists)))
  (table nil :type sql-symbol)
  (if-exists nil :type boolean))

@export
(defstruct (alter-table-statement (:include sql-statement (name "ALTER TABLE"))
                                  (:constructor make-alter-table-statement (table &rest children
                                                                            &aux (children
                                                                                  (apply #'make-sql-splicing-list children)))))
  (table nil :type sql-symbol)
  (children nil))

@export
(defstruct (create-index-statement (:include sql-statement (name "CREATE INDEX"))
                                   (:constructor make-create-index-statement (index-name table-name columns &key unique using if-not-exists)))
  (index-name nil :type sql-symbol)
  (table-name nil :type sql-symbol)
  (columns nil :type sql-list)
  (unique nil :type boolean)
  (using nil :type (or null sql-keyword))
  (if-not-exists nil :type boolean))

@export
(defstruct (drop-index-statement (:include sql-statement (name "DROP INDEX"))
                                 (:constructor make-drop-index-statement (index-name &key if-exists on)))
  (index-name nil :type sql-symbol)
  (if-exists nil :type boolean)
  (on nil :type (or null sql-symbol)))

@export
(defstruct (pragma-statement (:include sql-statement (name "PRAGMA"))
                             (:constructor make-pragma-statement (pragma-name &optional value)))
  "A statement for PRAGMA statement available in SQLITE. See https://www.sqlite.org/pragma.html"
  pragma-name
  value)

(defun find-make-statement (statement-name &optional (package *package*))
  (find-constructor statement-name #.(string :-statement)
                    :package package))

@export
(defgeneric make-statement (statement-name &rest args))

@export
(defmethod make-statement (statement-name &rest args)
  (apply (find-make-statement statement-name #.*package*)
         (remove nil (mapcar #'detect-and-convert args))))

(deftype multiple-allowed-clause () '(or join-clause where-clause))

@export
(defun merge-statements (statement defaults)
  (check-type statement select-statement)
  (check-type defaults select-statement)
  (apply #'make-statement :select
         (if defaults
             (iter (for type in '(fields-clause
                                  from-clause
                                  join-clause
                                  where-clause
                                  group-by-clause
                                  having-clause
                                  returning-clause
                                  order-by-clause
                                  limit-clause
                                  offset-clause))
               (appending
                (if (or (null defaults)
                        (slot-value statement type))
                    (if (subtypep type 'multiple-allowed-clause)
                        (append
                         (slot-value defaults type)
                         (slot-value statement type))
                        (slot-value statement type))
                    (slot-value defaults type)))))))

(defmethod make-statement ((statement-name (eql :select)) &rest args)
  (apply #'make-select-statement
         (iter (for (type clauses) on (group-by #'type-of
                                                args :test 'eq) :by #'cddr)
           (let ((type-key (intern (symbol-name type) :keyword)))
             (when (and (cdr clauses)
                        (not (subtypep type 'multiple-allowed-clause)))
               (error "Multiple ~S is not allowed." type))
             (collect type-key)
             (collect clauses)))))

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
       (nconc
        (mapcar #'(lambda (column)
                    (if (typep column 'column-definition-clause)
                        column
                        (apply #'make-column-definition-clause column)))
                column-definitions)
        options)))))

(defmethod make-statement ((statement-name (eql :drop-table)) &rest args)
  (destructuring-bind (table &key if-exists) args
    (make-drop-table-statement (detect-and-convert table)
                               :if-exists if-exists)))

(defmethod make-statement ((statement-name (eql :create-index)) &rest args)
  (destructuring-bind (index-name  &key unique using on if-not-exists) args
    (make-create-index-statement (detect-and-convert index-name)
                                 (detect-and-convert (car on))
                                 (apply #'make-sql-list
                                        (mapcar #'detect-and-convert (cdr on)))
                                 :unique unique
                                 :using (and using
                                             (make-sql-keyword (string using)))
                                 :if-not-exists if-not-exists)))

(defmethod make-statement ((statement-name (eql :drop-index)) &rest args)
  (destructuring-bind (index-name &key if-exists on) args
    (make-drop-index-statement (make-sql-symbol index-name)
                               :if-exists if-exists
                               :on (detect-and-convert on))))

(defmethod make-statement ((statement-name (eql :pragma)) &rest args)
  (apply #'make-pragma-statement args))

(defmethod yield ((statement create-table-statement))
  (with-yield-binds
    (format nil "~A~:[~; IF NOT EXISTS~] ~A (~%~{    ~A~^,~%~}~%)"
            (sql-statement-name statement)
            (create-table-statement-if-not-exists statement)
            (yield (create-table-statement-table statement))
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
   (format nil "CREATE~:[~; UNIQUE~] INDEX ~A~:[~; IF NOT EXISTS~]~:[~; USING ~:*~A~] ON ~A ~A"
           (create-index-statement-unique statement)
           (yield (create-index-statement-index-name statement))
           (create-index-statement-if-not-exists statement)
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

(defmethod yield ((statement pragma-statement))
  (values
   (format nil "PRAGMA ~A ~@[ = ~A~]"
           (pragma-statement-pragma-name statement)
           (pragma-statement-value statement))
   nil))


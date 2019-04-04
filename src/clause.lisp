(in-package :cl-user)
(defpackage sxql.clause
  (:use :cl
        :annot.class
        :sxql.sql-type
        :sxql.operator
        :trivial-types
        :iterate)
  (:import-from :sxql.sql-type
                :sql-symbol-name
                :sql-list-elements
                :expression-clause-expression)
  (:import-from :sxql.operator
                :=-op
                :as-op
                :as-op-right))
(in-package :sxql.clause)

(cl-syntax:use-syntax :annot)

@export
(defstruct (fields-clause (:include statement-clause (name ""))
                          (:constructor make-fields-clause (&rest fields
                                                            &aux (statement
                                                                  (apply #'make-sql-splicing-list fields))))))

@export
(defstruct (from-clause (:include statement-clause (name "FROM"))
                        (:constructor make-from-clause (&rest tables
                                                        &aux (statement
                                                              (apply #'make-sql-splicing-list tables))))))

@export
(defun from-clause-table-name (from)
  (let ((statements (sql-list-elements (from-clause-statement from))))
    (when (cdr statements)
      (error "Cannot tell the table name from the FROM clause that has multiple table names."))
    (etypecase (car statements)
      (sql-symbol (sql-symbol-name (car statements)))
      (as-op (sql-symbol-name (as-op-right (car statements)))))))

@export
(defstruct (where-clause (:include expression-clause (name "WHERE"))
                         (:constructor make-where-clause (expression))))

@export
(defun compose-where-clauses (clauses)
  (when clauses
    (make-where-clause
     (apply #'make-op
            :and
            (iter (for clause in clauses)
              (collect (expression-clause-expression clause)))))))

@export
(defstruct (order-by-clause (:include expression-list-clause (name "ORDER BY"))
                            (:constructor make-order-by-clause (&rest expressions))))

@export
(defstruct (limit-clause (:include expression-list-clause (name "LIMIT"))
                         (:constructor make-limit-clause (count1 &optional count2
                                                          &aux (expressions `(,count1 ,@(and count2 (list count2)))))))
  (count1 nil :type sql-variable)
  (count2 nil :type (or null sql-variable)))

@export
(defstruct (offset-clause (:include sql-clause (name "OFFSET"))
                          (:constructor make-offset-clause (offset)))
  (offset nil :type sql-variable))

@export
(defstruct (group-by-clause (:include expression-list-clause (name "GROUP BY"))
                            (:constructor make-group-by-clause (&rest expressions))))

@export
(defstruct (having-clause (:include expression-clause (name "HAVING"))
                          (:constructor make-having-clause (expression))))

@export
(defstruct (returning-clause (:include expression-clause (name "RETURNING"))
                          (:constructor make-returning-clause (expression))))

@export
(defstruct (updatability-clause (:include statement-clause)
                                (:constructor make-updatability-clause))
  (update-type :update :type keyword)
  (idents '() :type list)
  (nowait nil :type boolean))

(defmethod yield ((obj updatability-clause))
  (let ((params '()))
    (values (with-output-to-string (stream)
              (format stream "FOR ~A"
                      (ecase (updatability-clause-update-type obj)
                        (:update "UPDATE")
                        (:share "SHARE")))
              ;; Optional "OF <cols/tables>..." list.
              (when (updatability-clause-idents obj)
                (format stream " OF ")
                (multiple-value-bind (str params)
                    (yield (apply #'make-clause :fields (updatability-clause-idents obj)))
                  (write-string str stream)
                  (setf params params)))
              ;; Optional NOWAIT keyword
              (when (updatability-clause-nowait obj)
                (format stream " NOWAIT")))
            params)))

@export
(defstruct (join-clause (:include statement-clause)
                        (:constructor make-join-clause))
  (kind :inner :type (or (eql :inner)
                         (eql :left)
                         (eql :right)
                         (eql :full)))
  (on nil :type (or null sql-expression))
  (using nil :type (or null sql-symbol sql-list)))

@export
(defstruct (set=-clause (:include sql-clause (name "SET"))
                        (:constructor %make-set=-clause (&rest args)))
  (args nil :type (and proper-list
                     (satisfies sql-expression-list-p))))

(defun make-set=-clause (&rest args)
  (unless (and (cdr args)
               (= (mod (length args) 2) 0))
    ;; TODO: raise an error.
    )
  (apply #'%make-set=-clause args))

@export
(defstruct (key-clause (:include expression-clause (name "KEY"))
                       (:constructor make-key-clause (expression)))
  (key-name nil :type (or null sql-variable))
  (keys nil))

(defun make-key-clause-for-all (fn &rest key-args)
  (if (cdr key-args)
      (destructuring-bind (key-name keys) key-args
        (funcall fn
                 (make-sql-splicing-expression-list
                  (detect-and-convert key-name)
                  (apply #'make-sql-list
                         (mapcar #'detect-and-convert
                                 (if (listp keys)
                                     keys
                                     (list keys)))))))
      (let ((key-name (car key-args)))
        (funcall fn
                 (make-sql-splicing-expression-list
                  (apply #'make-sql-list
                         (mapcar #'detect-and-convert
                                 (if (listp key-name)
                                     key-name
                                     (list key-name)))))))))

(defmethod yield ((clause key-clause))
  (let ((*use-placeholder* nil))
    (call-next-method)))

@export
(defstruct (primary-key-clause (:include key-clause (name "PRIMARY KEY"))
                               (:constructor make-primary-key-clause (expression))))

@export
(defstruct (unique-key-clause (:include key-clause (name "UNIQUE"))
                              (:constructor make-unique-key-clause (expression))))

@export
(defstruct (references-clause (:include expression-clause (name "REFERENCES"))
                              (:constructor make-references-clause (table-name column-names
                                                                    &aux (expression
                                                                          (make-sql-splicing-expression-list table-name column-names)))))
  (table-name nil :type sql-symbol)
  (column-names nil :type sql-list))

(defstruct (on-clause (:include sql-clause (name)))
  (action nil :type string))
(defmethod yield ((clause on-clause))
  (values
   (format nil "~A ~A"
           (slot-value clause 'name)
           (on-clause-action clause))
   nil))
(defstruct (on-delete-clause (:include on-clause (name "ON DELETE"))))
(defstruct (on-update-clause (:include on-clause (name "ON UPDATE"))))

@export
(defstruct (foreign-key-clause (:include expression-clause (name "FOREIGN KEY"))
                               (:constructor make-foreign-key-clause (column-names references on-delete on-update
                                                                      &aux (expression
                                                                            (apply #'make-sql-splicing-expression-list
                                                                             column-names references
                                                                             (append
                                                                              (and on-delete
                                                                                   (list
                                                                                    (make-on-delete-clause :action on-delete)))
                                                                              (and on-update
                                                                                   (list
                                                                                    (make-on-update-clause :action on-update)))))))))
  (column-names nil :type sql-list)
  (references nil :type references-clause))

(defstruct (column-modifier-clause (:include expression-clause)
                                   (:constructor nil))
  (column-definition nil :type column-definition-clause)
  (after nil :type (or sql-symbol null))
  (first nil :type boolean))

(defun make-column-modifier-clause (fn old-column-name column-name &rest args
                                    &key type not-null default auto-increment unique primary-key
                                      after first)
  (declare (ignore type not-null default auto-increment unique primary-key))
  (let ((args (list (apply #'make-column-definition-clause column-name
                           (loop for (k v) on args by #'cddr
                                 unless (or (eq k :after) (eq k :first))
                                   append (list k v)))
                    :after (detect-and-convert after)
                    :first first)))
    (apply fn
           (if old-column-name
               (cons (detect-and-convert old-column-name) args)
               args))))

(defmethod yield ((clause column-modifier-clause))
  (with-yield-binds
    (format nil "~A~:[~; AFTER ~:*~A~]~:[~; FIRST~]"
            (call-next-method)
            (and (column-modifier-clause-after clause)
                 (yield (column-modifier-clause-after clause)))
            (column-modifier-clause-first clause))))

(defstruct (add-column-clause (:include column-modifier-clause (name "ADD COLUMN"))
                              (:constructor make-add-column-clause (column-definition
                                                                    &key after first
                                                                    &aux (expression
                                                                          (make-sql-splicing-expression-list column-definition))))))

(defstruct (modify-column-clause (:include column-modifier-clause (name "MODIFY COLUMN"))
                                 (:constructor make-modify-column-clause (column-definition
                                                                          &key after first
                                                                          &aux (expression
                                                                                (make-sql-splicing-expression-list column-definition))))))

(defstruct (alter-column-clause (:include sql-clause (name "ALTER COLUMN"))
                                (:constructor make-alter-column-clause (column-name
                                                                        &key type set-default drop-default not-null)))
  "Generates an ALTER COLUMN clause. This is PostgreSQL version of MODIFY COLUMN."
  column-name type set-default drop-default
  (not-null :unspecified))

(defmethod yield ((clause alter-column-clause))
  (with-yield-binds
    (format nil "ALTER COLUMN ~A~:[~; TYPE ~:*~A~]~:[~; SET DEFAULT ~:*~A~]~:[~; DROP DEFAULT~]~A"
            (yield (alter-column-clause-column-name clause))
            (and (alter-column-clause-type clause)
                 (let ((*use-placeholder* nil))
                   (yield (alter-column-clause-type clause))))
            (and (alter-column-clause-set-default clause)
                 (yield (alter-column-clause-set-default clause)))
            (alter-column-clause-drop-default clause)
            (cond
              ((eq (alter-column-clause-not-null clause) :unspecified) "")
              ((alter-column-clause-not-null clause) " SET NOT NULL")
              (T " DROP NOT NULL")))))

(defstruct (change-column-clause (:include column-modifier-clause (name "CHANGE COLUMN"))
                                 (:constructor make-change-column-clause (old-column-name column-definition
                                                                          &key after first
                                                                          &aux (expression
                                                                                (make-sql-splicing-expression-list old-column-name column-definition))))))

(defstruct (rename-to-clause (:include expression-clause (name "RENAME TO"))
                             (:constructor make-rename-to-clause (expression))))

(defstruct (drop-column-clause (:include expression-clause (name "DROP COLUMN"))
                               (:constructor make-drop-column-clause (expression))))

@export
(defstruct (column-definition-clause (:include sql-clause)
                                     (:constructor %make-column-definition-clause (column-name &key type not-null default auto-increment autoincrement unique primary-key)))
  column-name
  type
  not-null
  default
  auto-increment
  autoincrement
  unique
  primary-key)

@export
(defun make-column-definition-clause (column-name &rest args &key type not-null default auto-increment autoincrement unique primary-key)
  (declare (ignore type not-null default auto-increment autoincrement unique primary-key))
  (apply #'%make-column-definition-clause
         (detect-and-convert column-name)
         (loop for (key val) on args by #'cddr
               if (eq key :type)
                 append (list key (make-sql-column-type-from-list val))
               else
                 append (list key (detect-and-convert val)))))

(defmethod yield ((clause column-definition-clause))
  (with-yield-binds
    (with-output-to-string (s)
      (write-string
       (yield (column-definition-clause-column-name clause)) s)
      (when (column-definition-clause-type clause)
        (let ((*use-placeholder* nil))
          (format s " ~A"
                  (yield (column-definition-clause-type clause)))))
      (when (column-definition-clause-not-null clause)
        (write-string " NOT NULL" s))
      (when (column-definition-clause-default clause)
        (format s " DEFAULT ~A"
                (yield (column-definition-clause-default clause))))
      (when (column-definition-clause-auto-increment clause)
        (write-string " AUTO_INCREMENT" s))
      (when (column-definition-clause-unique clause)
        (write-string " UNIQUE" s))
      (when (column-definition-clause-primary-key clause)
        (write-string " PRIMARY KEY" s))
      (when (and (column-definition-clause-autoincrement clause)
                 (not (column-definition-clause-auto-increment clause)))
       (write-string " AUTOINCREMENT" s)))))

(defstruct (add-primary-key-clause (:include expression-clause (name "ADD PRIMARY KEY"))
                                   (:constructor make-add-primary-key-clause (expression))))

(defstruct (drop-primary-key-clause (:include sql-clause (name "DROP PRIMARY KEY"))
                                    (:constructor make-drop-primary-key-clause ())))

(defmethod yield ((clause drop-primary-key-clause))
  (declare (ignore clause))
  (values
   "DROP PRIMARY KEY"
   nil))

(defstruct (on-duplicate-key-update-clause (:include sql-clause (name "ON DUPLICATE KEY UPDATE"))
                                           (:constructor %make-on-duplicate-key-update-clause (&rest args)))
  (args nil :type (and proper-list
                     (satisfies sql-expression-list-p))))

(defun make-on-duplicate-key-update-clause (&rest args)
  (unless (and (cdr args)
               (= (mod (length args) 2) 0))
    ;; TODO: raise an error.
    )
  (apply #'%make-on-duplicate-key-update-clause args))

(defmethod yield ((clause on-duplicate-key-update-clause))
  (labels ((yield-arg (arg)
             (if arg
                 (yield arg)
                 "NULL")))
    (with-yield-binds
      (format nil "ON DUPLICATE KEY UPDATE ~{~A = ~A~^, ~}"
              (mapcar #'yield-arg (on-duplicate-key-update-clause-args clause))))))

(defun make-conflict-target (raw-target)
  (if (listp raw-target)
      (apply #'make-sql-list
             (mapcar #'detect-and-convert raw-target))
  (detect-and-convert raw-target)))

(defstruct (on-conflict-do-nothing-clause (:include sql-clause (name "ON CONFLICT DO NOTHING"))
                                          (:constructor make-on-conflict-do-nothing-clause (conflict-target)))
  (conflict-target nil :type (or sql-list sql-symbol)))

(defmethod yield ((clause on-conflict-do-nothing-clause))
  (format nil "ON CONFLICT~[ ON CONSTRAINT ~A~; ~A~;~] DO NOTHING"
          (cond ((eq (type-of (on-conflict-do-nothing-clause-conflict-target clause))
                     'sql-symbol)
                 0)
                ((sql-list-elements (on-conflict-do-nothing-clause-conflict-target clause))
                 1)
                (t 2))
          (yield (on-conflict-do-nothing-clause-conflict-target clause))))


(defstruct (on-conflict-do-update-clause (:include sql-clause (name "ON CONFLICT DO UPDATE"))
                                         (:constructor %make-on-conflict-do-update-clause (conflict-target update-set &optional where-condition)))
  (conflict-target nil :type (or sql-list sql-symbol))
  (update-set nil :type set=-clause)
  (where-condition nil :type (or null where-clause)))

(defun make-on-conflict-do-update-clause (conflict-target update-set &optional where-condition)
  (when (and (eq (type-of conflict-target) 'sql-list)
             (null (sql-list-elements conflict-target)))
    (error "ON-CONFLICT-DO-UPDATE requires inference specification or constraint name. For example, ON CONFLICT (column_name)."))
  (%make-on-conflict-do-update-clause conflict-target update-set where-condition))

@export
(defparameter *inside-insert-into* nil)

(defmethod yield ((clause on-conflict-do-update-clause))
  (let ((*inside-insert-into* nil))
    (with-yield-binds
      (format nil "ON CONFLICT ~:[~;ON CONSTRAINT ~]~A DO UPDATE ~A~@[ ~A~]"
              (eq (type-of (on-conflict-do-update-clause-conflict-target clause))
                  'sql-symbol)
              (yield (on-conflict-do-update-clause-conflict-target clause))
              (yield (on-conflict-do-update-clause-update-set clause))
              (if (on-conflict-do-update-clause-where-condition clause)
                  (yield (on-conflict-do-update-clause-where-condition clause)))))))

(defun find-make-clause (clause-name &optional (package *package*))
  (find-constructor clause-name #.(string :-clause)
                    :package package))

@export
(defgeneric make-clause (clause-name &rest args)
  (:method ((clause-name t) &rest args)
    (apply (find-make-clause clause-name #.*package*)
           (mapcar #'detect-and-convert args))))

(defmethod make-clause ((clause-name (eql :join)) &rest args)
  (destructuring-bind (statement &key kind on using) args
    (make-join-clause
     :statement (if (listp statement)
                    (apply #'make-sql-expression-list statement)
                    (detect-and-convert statement))
     :name (format nil "~A-JOIN" kind)
     :kind kind
     :on (detect-and-convert on)
     :using (typecase using
              (null nil)
              (list (apply #'make-sql-list
                           (mapcar #'detect-and-convert using)))
              (t (detect-and-convert using))))))

(defmethod make-clause ((clause-name (eql :updatability)) &rest args)
  (destructuring-bind (update-type &key of nowait) args
    (make-updatability-clause
     :update-type update-type
     :idents (if (not (listp of)) (list of) of)
     :nowait nowait)))

(defmethod make-clause ((clause-name (eql :key)) &rest args)
  (apply #'make-key-clause-for-all #'make-key-clause args))

(defmethod make-clause ((clause-name (eql :primary-key)) &rest args)
  (apply #'make-key-clause-for-all #'make-primary-key-clause args))

(defmethod make-clause ((clause-name (eql :unique-key)) &rest args)
  (apply #'make-key-clause-for-all #'make-unique-key-clause args))

(defmethod make-clause ((clause-name (eql :foreign-key)) &rest args)
  (destructuring-bind (column-names &key references on-delete on-update) args
    (destructuring-bind (target-table-name &rest target-column-names) references
      (make-foreign-key-clause
       (apply #'make-sql-list (mapcar #'detect-and-convert
                                      (if (listp column-names)
                                          column-names
                                          (list column-names))))
       (make-references-clause (detect-and-convert target-table-name)
                               (apply #'make-sql-list (mapcar #'detect-and-convert
                                                              target-column-names)))
       on-delete
       on-update))))

(defmethod make-clause ((clause-name (eql :add-column)) &rest args)
  (apply #'make-column-modifier-clause #'make-add-column-clause
         nil args))

(defmethod make-clause ((clause-name (eql :modify-column)) &rest args)
  (apply #'make-column-modifier-clause #'make-modify-column-clause
         nil args))

(defmethod make-clause ((clause-name (eql :change-column)) &rest args)
  (apply #'make-column-modifier-clause #'make-change-column-clause
         (mapcar #'detect-and-convert args)))

(defmethod make-clause ((clause-name (eql :alter-column)) &rest args)
  (destructuring-bind (column-name &key type set-default drop-default (not-null :unspecified)) args
    (make-alter-column-clause (detect-and-convert column-name)
                              :type (and type
                                         (make-sql-column-type-from-list type))
                              :set-default (detect-and-convert set-default)
                              :drop-default drop-default
                              :not-null not-null)))

(defmethod make-clause ((clause-name (eql :add-primary-key)) &rest args)
  (make-add-primary-key-clause (apply #'make-sql-list (mapcar #'detect-and-convert args))))

(defmethod make-clause ((clause-name (eql :on-conflict-do-nothing)) &rest args)
  (make-on-conflict-do-nothing-clause (make-conflict-target (car args))))

(defmethod make-clause ((clause-name (eql :on-conflict-do-update)) &rest args)
  (make-on-conflict-do-update-clause (make-conflict-target (car args))
                                     (cadr args)
                                     (caddr args)))

(defmethod yield ((clause limit-clause))
  (let ((*use-placeholder* nil))
    (call-next-method)))

(defmethod yield ((clause offset-clause))
  (let ((*use-placeholder* nil))
    (values
     (format nil "OFFSET ~A"
             (yield (offset-clause-offset clause)))
     nil)))

(defmethod yield ((clause join-clause))
  (with-yield-binds
    (values
     (format nil "~A JOIN ~A~:[~;~:* ON ~A~]~:[~;~:* USING ~A~]"
             (join-clause-kind clause)
             (yield (join-clause-statement clause))
             (if (join-clause-on clause)
                 (yield (join-clause-on clause))
                 nil)
             (if (join-clause-using clause)
                 (yield (join-clause-using clause))
                 nil)))))

(defmethod yield ((clause set=-clause))
  (labels ((yield-arg (arg)
             (if arg
                 (yield arg)
                 "NULL")))
    (with-yield-binds
      (if *inside-insert-into*
          (apply #'format nil
                 "(~{~A~^, ~}) VALUES (~{~A~^, ~})"
                 (loop for (k v) on (set=-clause-args clause) by #'cddr
                       collect (yield-arg k) into keys
                       collect (yield-arg v) into values
                       finally
                          (return (list keys values))))
          (format nil "SET ~{~A = ~A~^, ~}"
                  (mapcar #'yield-arg (set=-clause-args clause)))))))

(defun make-sql-column-type-from-list (val)
  (destructuring-bind (type &optional args &rest attrs)
      (if (listp val)
          val
          (list val))
    (make-sql-column-type
     type
     :args (mapcar #'detect-and-convert (if (listp args)
                                            args
                                            (list args)))
     :attrs (mapcar #'make-type-keyword attrs))))

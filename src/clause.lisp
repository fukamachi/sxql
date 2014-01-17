(in-package :cl-user)
(defpackage sxql.clause
  (:use :cl
        :annot.class
        :sxql.sql-type
        :sxql.operator
        :trivial-types)
  (:import-from :sxql.operator
                :=-op))
(in-package :sxql.clause)

(cl-syntax:use-syntax :annot)

@export
(defstruct (from-clause (:include statement-clause (name "FROM"))
                        (:constructor make-from-clause (statement))))

@export
(defstruct (where-clause (:include expression-clause (name "WHERE"))
                         (:constructor make-where-clause (expression))))

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
(defstruct (left-join-clause (:include statement-clause (name "LEFT JOIN"))
                             (:constructor make-left-join-clause))
  (on nil :type (or null =-op))
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

@export
(defstruct (foreign-key-clause (:include expression-clause (name "FOREIGN KEY"))
                               (:constructor make-foreign-key-clause (column-names references
                                                                      &aux (expression
                                                                            (make-sql-splicing-expression-list column-names references)))))
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
                                     (:constructor %make-column-definition-clause (column-name &key type not-null default auto-increment unique primary-key)))
  column-name
  type
  not-null
  default
  auto-increment
  unique
  primary-key)

@export
(defun make-column-definition-clause (column-name &rest args &key type not-null default auto-increment unique primary-key)
  (declare (ignore type not-null default auto-increment unique primary-key))
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
          (format s " ~:@(~A~)"
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
        (write-string " PRIMARY KEY" s)))))

(defstruct (add-primary-key-clause (:include expression-clause (name "ADD PRIAMRY KEY"))
                                   (:constructor make-add-primary-key-clause (&rest expression
                                                                              &aux (expression
                                                                                    (apply #'make-sql-list
                                                                                           expression))))))

(defun find-make-clause (clause-name &optional (package *package*))
  (find-constructor clause-name #.(string :-clause)
                    :package package))

@export
(defmethod make-clause (clause-name &rest args)
  (apply (find-make-clause clause-name #.*package*)
         (mapcar #'detect-and-convert args)))

(defmethod make-clause ((clause-name (eql :left-join)) &rest args)
  (destructuring-bind (statement &key on using) args
    (make-left-join-clause
     :statement (if (listp statement)
                    (apply #'make-sql-expression-list statement)
                    (detect-and-convert statement))
     :on (detect-and-convert on)
     :using (typecase using
              (null nil)
              (list (apply #'make-sql-list
                           (mapcar #'detect-and-convert using)))
              (t (detect-and-convert using))))))

(defmethod make-clause ((clause-name (eql :key)) &rest args)
  (apply #'make-key-clause-for-all #'make-key-clause args))

(defmethod make-clause ((clause-name (eql :primary-key)) &rest args)
  (apply #'make-key-clause-for-all #'make-primary-key-clause args))

(defmethod make-clause ((clause-name (eql :unique-key)) &rest args)
  (apply #'make-key-clause-for-all #'make-unique-key-clause args))

(defmethod make-clause ((clause-name (eql :foreign-key)) &rest args)
  (destructuring-bind (column-names &key references) args
    (destructuring-bind (target-table-name &rest target-column-names) references
      (make-foreign-key-clause
       (apply #'make-sql-list (mapcar #'detect-and-convert
                                      (if (listp column-names)
                                          column-names
                                          (list column-names))))
       (make-references-clause (detect-and-convert target-table-name)
                               (apply #'make-sql-list (mapcar #'detect-and-convert
                                                              target-column-names)))))))

(defmethod make-clause ((clause-name (eql :add-column)) &rest args)
  (apply #'make-column-modifier-clause #'make-add-column-clause
         nil args))

(defmethod make-clause ((clause-name (eql :modify-column)) &rest args)
  (apply #'make-column-modifier-clause #'make-modify-column-clause
         nil args))

(defmethod make-clause ((clause-name (eql :change-column)) &rest args)
  (apply #'make-column-modifier-clause #'make-change-column-clause
         args))

(defmethod make-clause ((clause-name (eql :alter-column)) &rest args)
  (destructuring-bind (column-name &key type set-default drop-default (not-null :unspecified)) args
    (make-alter-column-clause (detect-and-convert column-name)
                              :type (and type
                                         (make-sql-column-type-from-list type))
                              :set-default (detect-and-convert set-default)
                              :drop-default drop-default
                              :not-null not-null)))

(defmethod yield ((clause limit-clause))
  (let ((*use-placeholder* nil))
    (call-next-method)))

(defmethod yield ((clause offset-clause))
  (let ((*use-placeholder* nil))
    (values
     (format nil "OFFSET ~A"
             (yield (offset-clause-offset clause)))
     nil)))

(defmethod yield ((clause left-join-clause))
  (with-yield-binds
    (values
     (format nil "LEFT JOIN ~A~:[~;~:* ON ~A~]~:[~;~:* USING ~A~]"
             (yield (left-join-clause-statement clause))
             (if (left-join-clause-on clause)
                 (yield (left-join-clause-on clause))
                 nil)
             (if (left-join-clause-using clause)
                 (yield (left-join-clause-using clause))
                 nil)))))

@export
(defparameter *inside-insert-into* nil)

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
  (let ((unsignedp (and (listp val)
                        (cdr val)
                        (eq (car (last val)) :unsigned))))
    (if (listp val)
        (make-sql-column-type
         (car val)
         :unsigned unsignedp
         :args (mapcar #'detect-and-convert
                       (if unsignedp
                           (butlast (cdr val))
                           (cdr val))))
        (make-type-keyword val))))

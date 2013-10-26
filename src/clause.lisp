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
                             (:constructor %make-left-join-clause))
  (:on nil :type (or null =-op)))

@export
(defun make-left-join-clause (statement &key on)
  (%make-left-join-clause
   :statement (if (typep statement 'sql-list)
                  (apply #'make-sql-expression-list (sql-list-elements statement))
                  statement)
   :on on))

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
               if (and (eq key :type) (symbolp val))
                 append (list key (make-sql-keyword (string-upcase val)))
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

(defun find-make-clause (clause-name &optional (package *package*))
  (find-constructor clause-name #.(string :-clause)
                    :package package))

@export
(defun make-clause (clause-name &rest args)
  (apply (find-make-clause clause-name #.*package*)
         (mapcar #'detect-and-convert args)))

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
     (format nil "LEFT JOIN ~A~:[~;~:* ON ~A~]"
             (yield (left-join-clause-statement clause))
             (if (left-join-clause-on clause)
                 (yield (left-join-clause-on clause))
                 nil)))))

(defmethod yield ((clause set=-clause))
  (with-yield-binds
    (format nil "SET ~{~A = ~A~^, ~}"
            (mapcar #'yield (set=-clause-args clause)))))

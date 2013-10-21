(in-package :cl-user)
(defpackage sxql.clause
  (:use :cl
        :sxql.sql-type
        :sxql.operator))
(in-package :sxql.clause)

(cl-syntax:use-syntax :annot)

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

@export
(defstruct (left-join-clause (:include statement-clause (name "LEFT JOIN"))
                             (:constructor %make-left-join-clause))
  (:on nil :type (or null =-op)))

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
(defun make-left-join-clause (statement &key on)
  (%make-left-join-clause
   :statement (if (typep statement 'sql-list)
                  (apply #'make-sql-expression-list (sql-list-elements statement))
                  statement)
   :on on))

(defun find-make-clause (clause-name &optional (package *package*))
  (find-constructor clause-name #.(string :-clause)
                    :package package))

@export
(defun make-clause (clause-name &rest args)
  (apply (find-make-clause clause-name #.*package*)
         (mapcar #'detect-and-convert args)))

(defmethod yield ((clause limit-clause))
  (let ((*use-placeholder* nil))
    (values
     (format nil "LIMIT ~A~:[~;~:*, ~A~]"
             (yield (limit-clause-count1 clause))
             (and (limit-clause-count2 clause)
                  (yield (limit-clause-count2 clause))))
     nil)))

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

(in-package :cl-user)
(defpackage sxql.sql-type
  (:use :cl
        :annot.class
        :trivial-types
        :split-sequence))
(in-package :sxql.sql-type)

(cl-syntax:use-syntax :annot)

@export
(defparameter *quote-character* nil)

@export
(defparameter *use-placeholder* t)

@export
(defparameter *use-prin1-for-print-object* nil)

(defparameter *bind-values* nil)
(defparameter *use-global-bind-values* nil)

@export
(defmacro with-yield-binds (&body body)
  `(let ((*bind-values* nil)
         (*use-global-bind-values* t))
     (values
      (progn ,@body)
      (loop for bind in (reverse *bind-values*)
            append bind))))

;;
;; Atom

@export
(defstruct sql-atom)

@export
@export-constructors
(defstruct (sql-variable (:include sql-atom)
                         (:constructor make-sql-variable (value)))
  (value nil :type (or string number)))

@export
@export-constructors
(defstruct (sql-keyword (:include sql-atom)
                        (:constructor make-sql-keyword (name)))
  (name nil :type string))

@export
@export-constructors
(defstruct (sql-symbol (:include sql-atom)
                       (:constructor make-sql-symbol (name)))
  (name nil :type string))

@export 'elements
@export
@export-accessors
@export-constructors
(defstruct (sql-list (:constructor make-sql-list (&rest elements)))
  (elements nil :type proper-list))

;;
;; Operator

@export 'name
@export
(defstruct sql-op
  (name nil :type string))

@export
(deftype sql-expression () '(or sql-atom sql-list sql-op))

(defun sql-expression-p (object)
  (typep object 'sql-expression))

(defun sql-expression-list-p (object)
  (every #'sql-expression-p object))

@export
@export-constructors
(defstruct (sql-expression-list (:constructor make-sql-expression-list (&rest elements))
                                (:predicate nil))
  (elements nil :type (and proper-list
                         (satisfies sql-expression-list-p))))

@export 'var
@export
@export-constructors
(defstruct (unary-op (:include sql-op)
                     (:constructor make-unary-op (name var)))
  (var nil :type sql-expression))

@export
(defstruct (unary-postfix-op (:include unary-op)))

@export 'left @export 'right
@export
@export-constructors
(defstruct (infix-op (:include sql-op)
                     (:constructor make-infix-op (name left right)))
  (left nil :type (or sql-expression
                    sql-expression-list))
  (right nil :type (or sql-expression
                     sql-expression-list
                     sql-list)))

@export
@export-constructors
(defstruct (infix-list-op (:include sql-op))
  (left nil :type sql-expression)
  (right nil :type sql-list))

@export 'expressions
@export
@export-constructors
(defstruct (conjunctive-op (:include sql-op)
                           (:constructor make-conjunctive-op (name &rest expressions
                                                              &aux (expressions (apply #'make-sql-expression-list expressions)))))
  (expressions nil :type sql-expression-list))

@export
@export-constructors
(defstruct (function-op (:include conjunctive-op)
                        (:constructor make-function-op (name &rest expressions
                                                        &aux (expressions (apply #'make-sql-expression-list expressions))))))

;;
;; Clause

@export
(defstruct sql-clause
  (name nil :type string))

(defun sql-clause-list-p (object)
  (every #'sql-clause-p object))

@export
(deftype sql-clause-list ()
  '(and proper-list
        (satisfies sql-clause-list-p)))

@export 'expression
@export
(defstruct (expression-clause (:include sql-clause))
  (expression nil :type (or sql-expression
                           sql-expression-list
                           sql-list)))

@export 'statement
@export
(defstruct (statement-clause (:include sql-clause))
  (statement nil :type (or sql-expression
                         sql-expression-list
                         sql-statement)))

@export 'expressions
@export
(defstruct (expression-list-clause (:include sql-clause))
  (expressions nil :type (and proper-list
                            (satisfies sql-expression-list-p))))

(defmethod print-object ((clause sql-clause) stream)
  (format stream "#<SXQL-CLAUSE: ~A>"
          (let ((*use-placeholder* nil)
                (*use-prin1-for-print-object* t))
            (yield clause))))

;;
;; Statement

@export
(defstruct sql-statement
  (name nil :type string))

@export 'children
@export
(defstruct (sql-composed-statement (:include sql-statement))
  (children nil :type proper-list))

(defmethod print-object ((clause sql-statement) stream)
  (format stream "#<SXQL-STATEMENT: ~A>"
          (let ((*use-placeholder* nil)
                (*use-prin1-for-print-object* t))
            (yield clause))))

;;
;; Yield

@export
(defgeneric yield (object))

(defmethod yield ((symbol sql-symbol))
  (let ((format-string (format nil "~~{~A~~A~:*~A~~^.~~}"
                               (or *quote-character* ""))))
    (values
     (format nil format-string (split-sequence #\. (sql-symbol-name symbol)))
     nil)))

(defmethod yield ((keyword sql-keyword))
  (values
   (sql-keyword-name keyword)
   nil))

(defmethod yield ((var sql-variable))
  (if *use-placeholder*
      (values "?" (list (sql-variable-value var)))
      (values
       (if *use-prin1-for-print-object*
           (if (stringp (sql-variable-value var))
               (format nil "'~A'"
                       (sql-variable-value var))
               (prin1-to-string (sql-variable-value var)))
           (princ-to-string (sql-variable-value var)))
       nil)))

(defmethod yield ((list sql-list))
  (with-yield-binds
    (format nil "(~{~A~^, ~})"
            (mapcar #'yield
                    (sql-list-elements list)))))

(defmethod yield ((list sql-expression-list))
  (with-yield-binds
    (format nil "(~{~A~^ ~})"
            (mapcar #'yield (sql-expression-list-elements list)))))

(defmethod yield ((op unary-op))
  (multiple-value-bind (var binds)
      (yield (unary-op-var op))
    (values (format nil "(~A ~A)"
                    (sql-op-name op)
                    var)
            binds)))

(defmethod yield ((op unary-postfix-op))
  (multiple-value-bind (var binds)
      (yield (unary-op-var op))
    (values (format nil "~A ~A"
                    var
                    (sql-op-name op))
            binds)))

(defmethod yield ((op infix-op))
  (with-yield-binds
    (format nil "(~A ~A ~A)"
            (yield (infix-op-left op))
            (sql-op-name op)
            (yield (infix-op-right op)))))

(defmethod yield ((op infix-list-op))
  (yield
   (make-infix-op (sql-op-name op)
                  (infix-list-op-left op)
                  (infix-list-op-right op))))

(defmethod yield ((op conjunctive-op))
  (with-yield-binds
    (format nil (format nil "(~~{~~A~~^ ~A ~~})" (sql-op-name op))
            (mapcar #'yield (sql-expression-list-elements (conjunctive-op-expressions op))))))

(defmethod yield ((op function-op))
  (with-yield-binds
    (format nil "~A(~{~A~^, ~})"
            (sql-op-name op)
            (mapcar #'yield (sql-expression-list-elements (function-op-expressions op))))))

(defmethod yield ((clause expression-clause))
  (multiple-value-bind (sql bind)
      (yield (expression-clause-expression clause))
    (values
     (format nil "~A ~A"
             (sql-clause-name clause)
             sql)
     bind)))

(defmethod yield ((clause statement-clause))
  (with-yield-binds
    (format nil (if (sql-statement-p (statement-clause-statement clause))
                  "~A (~A)"
                  "~A ~A")
            (sql-clause-name clause)
            (yield (statement-clause-statement clause)))))

(defmethod yield ((clause expression-list-clause))
  (with-yield-binds
    (format nil "~A ~{~A~^, ~}"
            (sql-clause-name clause)
            (mapcar #'yield (expression-list-clause-expressions clause)))))

(defmethod yield ((statement sql-composed-statement))
  (with-yield-binds
    (format nil "~A ~{~A~^ ~}"
            (sql-statement-name statement)
            (mapcar #'yield (sql-composed-statement-children statement)))))

(defmethod yield :around ((object t))
  (if *use-global-bind-values*
      (progn
        (multiple-value-bind (var bind) (call-next-method)
          (when bind (push bind *bind-values*))
          (values var nil)))
      (call-next-method)))

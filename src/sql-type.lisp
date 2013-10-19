(in-package :cl-user)
(defpackage sxql.sql-type
  (:use :cl
        :annot.class
        :trivial-types
        :split-sequence))
(in-package :sxql.sql-type)

(cl-syntax:use-syntax :annot)

@export
(defparameter *use-placeholder* t)

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

(defun sql-atom-list-p (object)
  (every #'sql-atom-p object))

@export
@export-constructors
;; XXX: まだ使うことある？？
(defstruct (sql-atom-list (:constructor make-sql-atom-list (atoms))
                          (:predicate nil))
  (atoms nil :type (and proper-list
                      (satisfies sql-atom-list-p))))

@export
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
(deftype sql-expression () '(or sql-atom sql-atom-list sql-op))

(defun sql-expression-p (object)
  (typep object 'sql-expression))

(defun sql-expression-list-p (object)
  (every #'sql-expression-p object))

@export
(deftype sql-expression-list () '(and proper-list (satisfies sql-expression-list-p)))

@export 'var
@export
@export-constructors
(defstruct (unary-op (:include sql-op)
                     (:constructor make-unary-op (name var)))
  (var nil :type sql-expression))

@export
(defstruct (unary-suffix-op (:include unary-op)))

@export 'left @export 'right
@export
@export-constructors
(defstruct (infix-op (:include sql-op)
                     (:constructor make-infix-op (name left right)))
  (left nil :type (or sql-expression
                    sql-expression-list))
  (right nil :type (or sql-expression
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
                           (:constructor make-conjunctive-op (name &rest expressions)))
  (expressions nil :type sql-expression-list))

@export
@export-constructors
(defstruct (function-op (:include conjunctive-op)
                        (:constructor make-function-op (name &rest expressions))))

;;
;; Clause

@export
(defstruct sql-clause
  (name nil :type string))

@export 'expression
@export
(defstruct (expression-clause (:include sql-clause))
  (expression nil :type (or sql-expression
                          sql-list)))

@export 'statement
@export
(defstruct (statement-clause (:include sql-clause))
  (statement nil :type (or sql-expression sql-statement)))

;;
;; Statement

@export
(defstruct sql-statement
  (name nil :type string))

;;
;; Stringify

@export
(defgeneric stringify (op))

(defmethod stringify ((s sql-symbol))
  (values
   (format nil "~{`~A`~^.~}" (split-sequence #\. (sql-symbol-name s)))
   nil))

(defmethod stringify ((s sql-keyword))
  (values
   (sql-symbol-name s)
   nil))

(defmethod stringify ((var sql-variable))
  (if *use-placeholder*
      (values "?" (list (sql-variable-value var)))
      (values (princ-to-string (sql-variable-value var))
              nil)))

(defmethod stringify ((atom-list sql-atom-list))
  (stringify
   (apply #'make-sql-list
          (sql-atom-list-atoms atom-list))))

(defmethod stringify ((list sql-list))
  (multiple-value-bind (var binds)
      (merged-multiple-values #'stringify (sql-list-elements list))
    (values
     (format nil "(~{~A~^, ~})"
             var)
     binds)))

(defmethod stringify ((op unary-op))
  (multiple-value-bind (var binds)
      (stringify (unary-op-var op))
    (values (format nil "(~A ~A)"
                    (sql-op-name op)
                    var)
            binds)))

(defmethod stringify ((op unary-suffix-op))
  (multiple-value-bind (var binds)
      (stringify (unary-op-var op))
    (values (format nil "~A ~A"
                    var
                    (sql-op-name op))
            binds)))

(defmethod stringify ((op infix-op))
  (multiple-value-bind (var1 binds1)
      (stringify (infix-op-left op))
    (multiple-value-bind (var2 binds2) (stringify (infix-op-right op))
      (values
       (format nil "(~A ~A ~A)"
               var1
               (sql-op-name op)
               var2)
       (append binds1 binds2)))))

(defmethod stringify ((op infix-list-op))
  (stringify
   (make-infix-op (sql-op-name op)
                  (infix-list-op-left op)
                  (infix-list-op-right op))))

(defmethod stringify ((op conjunctive-op))
  (multiple-value-bind (vars bind)
      (merged-multiple-values #'stringify (conjunctive-op-expressions op))
    (values
     (format nil (format nil "(~~{~~A~~^ ~A ~~})" (sql-op-name op))
             vars)
     bind)))

(defmethod stringify ((op function-op))
  (multiple-value-bind (vars bind)
      (merged-multiple-values #'stringify (function-op-expressions op))
    (values
     (format nil "~A(~{~A~^, ~})"
             (sql-op-name op)
             vars)
     bind)))

(defmethod stringify ((clause expression-clause))
  (multiple-value-bind (sql bind)
      (stringify (expression-clause-expression clause))
    (values
     (format nil "~A ~A"
             (sql-clause-name clause)
             sql)
     bind)))

(defmethod stringify ((clause statement-clause))
  (stringify
   (make-expression-clause
    :name (sql-clause-name clause)
    :expression (statement-clause-statement clause))))

(defun merged-multiple-values (func list-of-forms)
  (loop for form in list-of-forms
        for (sql binds) = (multiple-value-list (funcall func form))
        collect sql into sqls
        append binds into binds-list
        finally (return (values sqls binds-list))))

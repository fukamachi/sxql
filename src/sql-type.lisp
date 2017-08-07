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

(defparameter *bind-values* nil)
(defparameter *use-global-bind-values* nil)
(defparameter *inside-function-op* nil)

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
@export-accessors
@export-constructors
(defstruct (sql-variable (:include sql-atom)
                         (:constructor make-sql-variable (value)))
  (value nil :type (or string number (vector (unsigned-byte 8)) array)))

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

@export-constructors
(defstruct (sql-splicing-list (:include sql-list)
                              (:constructor make-sql-splicing-list (&rest elements))))

@export 'name
@export
(defstruct sql-op
  (name nil :type string))

(defmethod print-object ((op sql-op) stream)
  (format stream "#<SXQL-OP: ~A>"
          (let ((*use-placeholder* nil))
            (yield op))))

@export
@export-constructors
(defstruct (sql-column-type (:constructor make-sql-column-type (name &key args attrs
                                                                &aux (name (make-type-keyword name)))))
  (name nil)
  (args nil :type list)
  (attrs nil :type list))

@export
(defun make-type-keyword (type)
  (typecase type
    (string (make-sql-keyword type))
    (symbol (make-sql-keyword (string-upcase type)))
    (t type)))

@export
(deftype sql-expression () '(or sql-atom sql-list sql-op sql-clause null))

(defun sql-expression-p (object)
  (typep object 'sql-expression))

@export
(defun sql-expression-list-p (object)
  (every #'sql-expression-p object))

@export
@export-constructors
(defstruct (sql-expression-list (:constructor make-sql-expression-list (&rest elements))
                                (:predicate nil))
  (elements nil :type (and proper-list
                         (satisfies sql-expression-list-p))))

@export
@export-constructors
(defstruct (sql-splicing-expression-list (:include sql-expression-list)
                                         (:constructor make-sql-splicing-expression-list (&rest elements))))

@export
(defstruct sql-clause
  (name "" :type string))

(defun sql-clause-list-p (object)
  (every #'sql-clause-p object))

@export
(deftype sql-clause-list ()
  '(and proper-list
        (satisfies sql-clause-list-p)))

@export
@export-accessors
(defstruct sql-statement
  (name "" :type string))

(defun sql-statement-list-p (object)
  (every #'(lambda (element)
             (typep element 'sql-all-type))
         object))

(deftype sql-all-type () '(or sql-expression sql-statement))

;;
;; Operator

@export 'var
@export
@export-constructors
(defstruct (unary-op (:include sql-op)
                     (:constructor make-unary-op (name var)))
  (var nil :type sql-expression))

@export
@export-constructors
(defstruct (unary-splicing-op (:include unary-op)
                              (:constructor make-unary-splicing-op (name var))))

@export
(defstruct (unary-postfix-op (:include unary-op)))

@export 'left @export 'right
@export
@export-constructors
(defstruct (infix-op (:include sql-op)
                     (:constructor make-infix-op (name left right)))
  (left nil :type (or sql-statement
                    sql-expression
                    sql-expression-list))
  (right nil :type (or sql-statement
                     sql-expression
                     sql-expression-list)))

@export
@export-constructors
(defstruct (infix-splicing-op (:include infix-op)
                              (:constructor make-infix-splicing-op (name left right))))

@export
@export-constructors
(defstruct (infix-list-op (:include sql-op))
  (left nil :type sql-expression)
  (right nil :type (or proper-list
                       sql-statement)))

@export 'expressions
@export
@export-constructors
(defstruct (conjunctive-op (:include sql-op)
                           (:constructor make-conjunctive-op (name &rest expressions)))
  (expressions nil :type (and proper-list
                            (satisfies sql-statement-list-p))))

@export
@export-constructors
(defstruct (function-op (:include conjunctive-op)
                        (:constructor make-function-op (name &rest expressions))))

;;
;; Clause

@export 'expression
@export
(defstruct (expression-clause (:include sql-clause))
  (expression nil :type (or sql-expression
                           sql-expression-list)))

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
          (let ((*use-placeholder* nil))
            (yield clause))))

;;
;; Statement

@export 'children
@export
@export-accessors
(defstruct (sql-composed-statement (:include sql-statement))
  (children nil :type proper-list))

(defmethod print-object ((clause sql-statement) stream)
  (format stream "#<SXQL-STATEMENT: ~A>"
          (let ((*use-placeholder* nil))
            (yield clause))))

;;
;; Yield

@export
(defgeneric yield (object))

(defparameter *table-name-scope* nil)
@export
(defmacro with-table-name (table-name &body body)
  `(let ((*table-name-scope* ,table-name))
     ,@body))

(defmethod yield ((symbol sql-symbol))
  (let ((tokens (split-sequence #\. (sql-symbol-name symbol))))
    (when (and *table-name-scope*
               (null (cdr tokens)))
      (push *table-name-scope* tokens))
    (values
     (loop for token in tokens
           if (string= token "*")
             collect token into tokens
           else
             collect (format nil "~A~A~A"
                             (or *quote-character* "")
                             token
                             (or *quote-character* "")) into tokens
           finally
              (return (format nil "~{~A~^.~}" tokens)))
     nil)))

(defmethod yield ((keyword sql-keyword))
  (values
   (sql-keyword-name keyword)
   nil))

(defmethod yield ((var sql-variable))
  (if *use-placeholder*
      (values "?" (list (sql-variable-value var)))
      (values
       (if (stringp (sql-variable-value var))
           (format nil "'~A'"
                   (sql-variable-value var))
           (princ-to-string (sql-variable-value var)))
       nil)))

(defmethod yield ((list sql-list))
  (with-yield-binds
    (format nil "(~A)"
            (yield
             (apply #'make-sql-splicing-list
                    (sql-list-elements list))))))

(defmethod yield ((list sql-splicing-list))
  (with-yield-binds
    (format nil "~{~A~^, ~}"
            (mapcar (lambda (element)
                      (if (sql-statement-p element)
                          (format nil "(~A)" (yield element))
                          (yield element)))
                    (sql-list-elements list)))))

(defmethod yield ((list sql-expression-list))
  (with-yield-binds
    (format nil "(~{~A~^ ~})"
            (mapcar #'yield (sql-expression-list-elements list)))))

(defmethod yield ((list sql-splicing-expression-list))
  (with-yield-binds
    (format nil "~{~A~^ ~}"
            (mapcar #'yield (sql-expression-list-elements list)))))

(defmethod yield ((op unary-op))
  (multiple-value-bind (var binds)
      (yield (unary-op-var op))
    (values (format nil "(~A ~A)"
                    (sql-op-name op)
                    var)
            binds)))

(defmethod yield ((op unary-splicing-op))
  (multiple-value-bind (var binds)
      (yield (unary-op-var op))
    (values (format nil "~A ~A"
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
            (if (sql-statement-p (infix-op-right op))
                (format nil "(~A)" (yield (infix-op-right op)))
                (yield (infix-op-right op))))))

(defmethod yield ((op infix-splicing-op))
  (with-yield-binds
    (format nil "~A ~A ~A"
            (if (sql-statement-p (infix-op-left op))
                (yield (make-sql-list (infix-op-left op)))
                (yield (infix-op-left op)))
            (sql-op-name op)
            (if (sql-statement-p (infix-op-right op))
                (yield (make-sql-list (infix-op-right op)))
                (yield (infix-op-right op))))))

(defmethod yield ((op infix-list-op))
  (with-yield-binds
    (format nil "(~A ~A ~A)"
            (yield (infix-list-op-left op))
            (sql-op-name op)
            (if (sql-statement-p (infix-list-op-right op))
                (format nil "(~A)" (yield (infix-list-op-right op)))
                (yield (apply #'make-sql-list (infix-list-op-right op)))))))

(defmethod yield ((op conjunctive-op))
  (with-yield-binds
    (if (cdr (conjunctive-op-expressions op))
        (format nil (format nil "(~~{~~A~~^ ~A ~~})" (sql-op-name op))
                (mapcar #'yield (conjunctive-op-expressions op)))
        (yield (car (conjunctive-op-expressions op))))))

(defmethod yield ((op function-op))
  (let ((*inside-function-op* t))
    (with-yield-binds
      (format nil "~A(~{~A~^, ~})"
              (sql-op-name op)
              (mapcar #'yield (function-op-expressions op))))))

(defmethod yield ((type sql-column-type))
  (let ((*use-placeholder* nil)
        (args (sql-column-type-args type)))
    (format nil "~A~:[~;~:*(~{~A~^, ~})~]~{ ~A~}"
            (yield (sql-column-type-name type))
            (mapcar #'yield args)
            (mapcar #'yield (sql-column-type-attrs type)))))

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
                  "~:[~A ~;~*~](~A)"
                  "~:[~A ~;~*~]~A")
            (string= (sql-clause-name clause) "")
            (sql-clause-name clause)
            (yield (statement-clause-statement clause)))))

(defmethod yield ((clause expression-list-clause))
  (with-yield-binds
    (format nil "~A ~{~A~^, ~}"
            (sql-clause-name clause)
            (mapcar #'yield (expression-list-clause-expressions clause)))))

(defmethod yield ((statement sql-composed-statement))
  (with-yield-binds
    (format nil (if *inside-function-op*
                    "(~A ~{~A~^ ~})"
                    "~A ~{~A~^ ~}")
            (sql-statement-name statement)
            (mapcar #'yield (sql-composed-statement-children statement)))))

(defmethod yield :around ((object t))
  (if *use-global-bind-values*
      (progn
        (multiple-value-bind (var bind) (call-next-method)
          (when bind (push bind *bind-values*))
          (values var nil)))
      (call-next-method)))

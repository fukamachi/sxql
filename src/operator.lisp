(in-package :cl-user)
(defpackage sxql.operator
  (:use :cl
        :sxql.sql-type)
  (:import-from :sxql.sql-type
                :sql-statement-p))
(in-package :sxql.operator)

(cl-syntax:use-syntax :annot)

(defmacro define-op ((op-name struct-type &key sql-op-name include-slots) &body body)
  (check-type op-name symbol)
  (let ((struct-name (intern (concatenate 'string (symbol-name op-name) #.(string :-op)))))
    `(defstruct (,struct-name (:include ,struct-type
                               (name ,(or sql-op-name
                                          (with-output-to-string (s)
                                            (loop for c across (symbol-name op-name)
                                                  if (char= c #\-)
                                                    do (write-char #\Space s)
                                                  else
                                                    do (write-char c s)))))
                               ,@include-slots)
                              (:constructor ,(intern (concatenate 'string
                                                                  #.(string :make-)
                                                                  (symbol-name op-name)
                                                                  #.(string :-op)))
                                  ,(ecase struct-type
                                     ((unary-op
                                       unary-splicing-op
                                       unary-postfix-op) '(var))
                                     ((infix-op
                                       infix-splicing-op
                                       infix-list-op) '(left right))
                                     (conjunctive-op '(&rest expressions)))))
       ,@body)))

(define-op (:not unary-op))
(define-op (:is-null unary-op))
(define-op (:not-null unary-op))
(define-op (:desc unary-postfix-op))
(define-op (:asc unary-postfix-op))
(define-op (:distinct unary-splicing-op))
(defstruct (on-op (:include sql-op (name "ON"))
                  (:constructor make-on-op (var)))
  (var nil :type =-op))

(define-op (:= infix-op))
(define-op (:!= infix-op))
(define-op (:< infix-op))
(define-op (:> infix-op))
(define-op (:<= infix-op))
(define-op (:>= infix-op))
(define-op (:as infix-splicing-op))
(define-op (:in infix-list-op))
(define-op (:not-in infix-list-op))
(define-op (:like infix-op))

(define-op (:or conjunctive-op))
(define-op (:and conjunctive-op))
(define-op (:+ conjunctive-op))
(define-op (:- conjunctive-op :sql-op-name "-"))
(define-op (:* conjunctive-op))
(define-op (:/ conjunctive-op))
(define-op (:% conjunctive-op))
(define-op (:union conjunctive-op))
(define-op (:union-all conjunctive-op))

(defstruct (raw-op (:include sql-op (name ""))
                   (:constructor make-raw-op (var)))
  (var nil :type (or string
                    sql-variable)))

@export
(defun find-constructor (name suffix &key (package *package*) (errorp t))
  (check-type name symbol)
  (let ((func-symbol (intern
                      (concatenate 'string
                                   #.(string :make-)
                                   (symbol-name name)
                                   suffix)
                      package)))
    (if (or errorp (fboundp func-symbol))
        (symbol-function func-symbol)
        nil)))

(defun find-make-op (op-name &optional (package *package*))
  (or (find-constructor op-name #.(string :-op)
                        :package package
                        :errorp nil)
      #'(lambda (&rest expressions)
          (apply #'make-function-op (symbol-name op-name) expressions))))

@export
(defun make-op (op-name &rest args)
  (apply (find-make-op op-name #.*package*)
         (mapcar #'detect-and-convert args)))

@export
(defun detect-and-convert (object)
  (etypecase object
    ((or number
         string
         (vector (unsigned-byte 8)))
     (make-sql-variable object))
    (boolean object)
    (symbol
     (let ((name (symbol-name object)))
       (if (string-equal name "null")
           (make-sql-keyword name)
           (make-sql-symbol (string-downcase object)))))
    (list
     (if (keywordp (car object))
         (apply #'make-op object)
         (mapcar #'detect-and-convert object)))
    (structure-object object)
    (standard-object (make-sql-variable (princ-to-string object)))))

(defmethod yield ((op is-null-op))
  (yield
   (make-infix-op "IS"
                  (is-null-op-var op)
                  (make-sql-keyword "NULL"))))

(defmethod yield ((op not-null-op))
  (yield
   (make-infix-op "IS NOT"
                  (not-null-op-var op)
                  (make-sql-keyword "NULL"))))

(defmethod yield ((raw raw-op))
  (values
   (format nil "(~A)"
           (etypecase (raw-op-var raw)
             (string (raw-op-var raw))
             (sql-variable (let ((*use-placeholder* nil))
                             (sql-variable-value (raw-op-var raw))))))
   nil))

(defmethod yield ((op as-op))
  (with-yield-binds
    (format nil "~A AS ~A"
            (if (sql-statement-p (as-op-left op))
                (yield (make-sql-list (as-op-left op)))
                (yield (as-op-left op)))
            (with-table-name nil
              (yield (as-op-right op))))))

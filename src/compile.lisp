(in-package :cl-user)
(defpackage sxql.compile
  (:use :cl
        :trivial-types
        :sxql.sql-type))
(in-package :sxql.compile)

(cl-syntax:use-syntax :annot)

(defgeneric find-compile-function (object))

(defmacro define-compile-struct (structure-name &rest defstruct-options)
  (flet ((symbolcat (&rest symbols)
           (intern (format nil "~{~A~^-~}" symbols) #.*package*)))
    (let ((compiled-name (symbolcat structure-name :compiled)))
      `(progn
         (defstruct (,compiled-name (:include ,structure-name ,@defstruct-options))
           (sql nil :type string)
           (bind nil :type proper-list))

         (defmethod find-compile-function ((object ,structure-name))
           (function ,(symbolcat :make compiled-name)))

         (defmethod print-object ((object ,compiled-name) stream)
           (format stream "#<SXQL-COMPILED: ~A [~{~A~^, ~}]>"
                   (,(symbolcat compiled-name :sql) object)
                   (,(symbolcat compiled-name :bind) object)))

         (defmethod yield ((object ,compiled-name))
           (values
            (,(symbolcat compiled-name :sql) object)
            (,(symbolcat compiled-name :bind) object)))))))

(define-compile-struct sql-op (name ""))
(define-compile-struct sql-clause)
(define-compile-struct sql-statement)

@export
(defun sql-compile (object)
  (multiple-value-bind (sql bind) (yield object)
    (funcall
     (find-compile-function object)
     :sql sql
     :bind bind)))

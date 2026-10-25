(defpackage #:sxql/compile
  (:nicknames #:sxql.compile)
  (:use #:cl
        #:sxql/sql-type)
  (:export
   ;; Functions
   #:sql-compile)
  (:import-from #:sxql/composer
                #:select-query-state))
(in-package #:sxql/compile)

(cl-package-locks:lock-package '#:sxql/compile)

(defgeneric find-compile-function (object))

(defmacro define-compile-struct (structure-name &rest defstruct-options)
  (flet ((symbolcat (&rest symbols)
           ;; STRING, not ~A of the symbol: *PRINT-CASE* must not change the name.
           (intern (format nil "~{~A~^-~}" (mapcar #'string symbols)) #.*package*)))
    (let ((compiled-name (symbolcat structure-name :compiled)))
      `(progn
         (defstruct (,compiled-name (:include ,structure-name ,@defstruct-options))
           (sql nil :type string)
           (bind nil :type list))

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
(define-compile-struct sxql/composer:select-query-state)

(defun sql-compile (object)
  (multiple-value-bind (sql bind) (yield object)
    (funcall
     (find-compile-function object)
     :sql sql
     :bind bind)))

(in-package :cl-user)
(defpackage sxql.statement
  (:use :cl
        :sxql.sql-type
        :sxql.clause)
  (:import-from :sxql.operator
                :find-constructor))
(in-package :sxql.statement)

(cl-syntax:use-syntax :annot)

@export
(defstruct (select-statement (:include sql-statement (name "SELECT")))
  (field nil :type (or null field-clause))
  (from nil :type (or null from-clause))
  (where nil :type (or null where-clause))
  (order-by nil :type (or null order-by-clause))
  (group-by nil :type (or null group-by-clause))
  (limit nil :type (or null limit-clause))
  (offset nil :type (or null offset-clause)))

(defun find-make-statement (statement-name &optional (package *package*))
  (find-constructor statement-name #.(string :-statement)
                    :package package))

@export
(defun make-statement (statement-name &rest args)
  (apply (find-make-statement statement-name #.*package*)
         args))

(defmethod stringify ((statement select-statement))
  (let (all-binds)
    (flet ((do-stringify (object)
             (multiple-value-bind (var bind) (stringify object)
               (push bind all-binds)
               var)))
      (values
       (with-output-to-string (s)
         (write-string "SELECT " s)

         ;; field
         (princ (if (select-statement-field statement)
                    (do-stringify (select-statement-field statement))
                    "*") s)

         (loop for slot in '(from where order-by group-by limit offset)
               when (slot-value statement slot)
                 do (write-char #\Space s)
                    (princ (do-stringify (slot-value statement slot)) s)))
       (loop for binds in (nreverse all-binds)
             append binds)))))

;; TODO
;; DESC, ASC

;; TODO
;; update, insert, delete, create table...

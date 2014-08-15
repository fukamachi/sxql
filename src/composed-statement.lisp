(in-package :cl-user)
(defpackage sxql.composed-statement
  (:use :cl
        :iterate)
  (:import-from :sxql.sql-type
                :*use-placeholder*
                :with-table-name
                :with-yield-binds
                :yield
                :sql-symbol
                :sql-symbol-name
                :sql-statement-name
                :sql-splicing-list-elements
                :statement-clause-statement
                :expression-clause-expression
                :expression-list-clause-expressions)
  (:import-from :sxql.operator
                :as-op
                :as-op-right)
  (:import-from :sxql.clause
                :fields-clause
                :from-clause
                :join-clause
                :where-clause
                :group-by-clause
                :order-by-clause
                :limit-clause
                :make-clause
                :fields-clause-statement)
  (:import-from :sxql.statement
                :select-statement
                :select-statement-table-name
                :select-statement-clause-order
                :compute-select-statement-children
                :sort-clause-types
                :make-statement)
  (:import-from :sxql.util
                :group-by
                :subdivide)
  (:import-from :alexandria
                :compose))
(in-package :sxql.composed-statement)

(cl-syntax:use-syntax :annot)

(defparameter *clause-delimiters*
  '((fields-clause . ", ")
    (from-clause . ", ")
    (where-clause . " AND ")
    (order-by-clause . ", ")))

(defgeneric yield-only-contents (clause)
  (:method (clause)
    (yield clause))
  (:method ((clause from-clause))
    (yield (statement-clause-statement clause)))
  (:method ((clause where-clause))
    (yield (expression-clause-expression clause)))
  (:method ((clause order-by-clause))
    (format nil "~{~A~^, ~}"
            (mapcar #'yield
                    (expression-list-clause-expressions clause)))))

(defgeneric merging-yield (clause-a clause-b &key table-name-a table-name-b))

(defmethod merging-yield ((clause-a null) clause-b &key table-name-a table-name-b)
  (declare (ignore clause-a table-name-a))
  (with-table-name table-name-b
    (yield clause-b)))

(defmethod merging-yield ((clause-a string) clause-b &key table-name-a table-name-b)
  (declare (ignore table-name-a))
  (format nil "~A~A~A"
          clause-a
          (or (cdr (assoc (type-of clause-b) *clause-delimiters*))
              " ")
          (with-table-name table-name-b
            (yield-only-contents clause-b))))

@export
(defstruct (composed-statement (:constructor make-composed-statement (&rest statements)))
  (statements nil))

(defmethod print-object ((statement composed-statement) stream)
  (format stream "#<SXQL-STATEMENT: ~A>"
          (let ((*use-placeholder* nil))
            (yield statement))))

(defstruct (scoped-clause (:constructor make-scoped-clause (clause statement)))
  clause
  statement)

(defun scoped-clause-type (scoped-clause)
  (type-of (scoped-clause-clause scoped-clause)))

(defun scoped-merging-yield (clause-a clause-b &key with-table-names)
  (let ((non-scope-clause-p (typep (scoped-clause-clause clause-b) '(or from-clause join-clause))))
    (merging-yield (if (scoped-clause-p clause-a)
                       (scoped-clause-clause clause-a)
                       clause-a)
                   (scoped-clause-clause clause-b)
                   :table-name-a (and (not non-scope-clause-p)
                                      with-table-names
                                      clause-a
                                      (not (stringp clause-a))
                                      (select-statement-table-name (scoped-clause-statement clause-a)))
                   :table-name-b (and (not non-scope-clause-p)
                                      with-table-names
                                      (select-statement-table-name (scoped-clause-statement clause-b))))))

(defmethod yield ((statement composed-statement))
  (unless (composed-statement-statements statement)
    (return-from yield (values "" nil)))

  (flet ((sort-plist-by-key (plist pred &key key)
           (apply #'nconc
                  (sort (subdivide plist 2)
                        pred
                        :key (compose key #'car)))))
    (let* ((has-join-clause-p nil)
           (clause-orders (sort-clause-types
                           (iter (for stmt in (composed-statement-statements statement))
                             (appending (select-statement-clause-order stmt)))))
           (grouped-clauses (sort-plist-by-key
                             (group-by #'scoped-clause-type
                                       (iter (for stmt in (composed-statement-statements statement))
                                         (appending
                                          (iter (for child in (compute-select-statement-children stmt))
                                            (when (typep child 'join-clause)
                                              (setf has-join-clause-p t))
                                            (collect (make-scoped-clause child stmt))))))
                             (lambda (a b)
                               (and a b
                                   (< a b)))
                             :key (lambda (type)
                                    (position type clause-orders)))))
      (with-yield-binds
        (format nil "~A ~{~A~^ ~}"
                (sql-statement-name (car (composed-statement-statements statement)))
                (iter (for (type scoped-clauses) on grouped-clauses :by #'cddr)
                  (when (and (eq type 'fields-clause)
                             (some (lambda (clause)
                                     (and (null (select-statement-table-name (scoped-clause-statement clause)))
                                          (some (lambda (field)
                                                  (string= "*"
                                                           (typecase field
                                                             (as-op (sql-symbol-name (as-op-right field)))
                                                             (sql-symbol (sql-symbol-name field)))))
                                                (sql-splicing-list-elements
                                                 (fields-clause-statement (scoped-clause-clause clause))))))
                                   scoped-clauses))
                    (setf scoped-clauses (list (make-scoped-clause
                                                (make-clause :fields :*)
                                                (make-statement :select)))))
                  (setf scoped-clauses
                        (delete-duplicates scoped-clauses
                                           :key #'scoped-clause-clause
                                           :test #'equalp
                                           :from-end t))
                  (collect
                      (reduce (if has-join-clause-p
                                  (lambda (a b)
                                    (scoped-merging-yield a b :with-table-names t))
                                  #'scoped-merging-yield)
                              scoped-clauses
                              :initial-value nil))))))))

@export
(defun compose-statements (statement &rest statements)
  (let ((statements (cons statement statements)))
    (mapc (lambda (stmt)
            (check-type stmt (or select-statement
                                 composed-statement)))
          statements)

    (apply #'make-composed-statement
           (mapcan
            (lambda (stmt)
              (if (composed-statement-p stmt)
                  (composed-statement-statements stmt)
                  (list stmt)))
            statements))))

(in-package :cl-user)
(defpackage sxql.operator
  (:use :cl
        :sxql.sql-type)
  (:import-from :sxql.sql-type
                :sql-statement-p
                :conjunctive-op-expressions
                :sql-all-type))
(in-package :sxql.operator)

(cl-syntax:use-syntax :annot)

@export
(defparameter *inside-select* nil)

(defmacro define-op ((op-name struct-type &key sql-op-name include-slots (package (find-package :sxql.operator))) &body body)
  (check-type op-name symbol)
  (let ((struct-name (intern (concatenate 'string (symbol-name op-name) #.(string :-op)) package)))
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
                                                                  #.(string :-op))
                                                     package)
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
(define-op (:a< infix-op :sql-op-name "@<"))
(define-op (:a> infix-op :sql-op-name "@>"))
(define-op (:as infix-splicing-op))
(define-op (:in infix-list-op))
(define-op (:not-in infix-list-op))
(define-op (:like infix-op))
(define-op (:is-distinct-from infix-op))
(define-op (:is-not-distinct-from infix-op))

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

(defstruct (splicing-raw-op (:include raw-op)
                            (:constructor make-splicing-raw-op (var))))

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

(defun has-lower-case-letters-p (symbol)
  "Take in a symbol, convert to string, look for presences of lower
case letters."
  (flet ((upper-or-not-alpha (c)
           (or (upper-case-p c) (not (alpha-char-p c)))))
    (not (every #'upper-or-not-alpha (symbol-name symbol)))))

@export
(defvar *sql-symbol-conversion* #'identity
  "Function for converting a string into an SQL symbol. It takes a string and must returns a string.")

@export
(defun detect-and-convert (object)
  (convert-for-sql object))

@export
(defgeneric convert-for-sql (object)
  (:method ((object number))
    (make-sql-variable object))
  (:method ((object string))
    (make-sql-variable object))
  (:method ((object vector))
    (make-sql-variable object))
  (:method ((object null))
    object)
  (:method ((object (eql t)))
    object)
  (:method ((object symbol))
    (let ((name (symbol-name object))
          (string-fn (if (has-lower-case-letters-p object) ;; Only downcase all caps
                         #'string
                         #'string-downcase)))
      (if (string-equal name "null")
          (make-sql-keyword name)
          (make-sql-symbol (funcall *sql-symbol-conversion* (funcall string-fn object))))))
  (:method ((object list))
    (if (keywordp (car object))
        (apply #'make-op object)
        (mapcar #'detect-and-convert object)))
  (:method ((object structure-object))
    (if (typep object 'sql-all-type)
        object
        (make-sql-variable (princ-to-string object))))
  (:method ((object standard-object))
    (if (string-equal (prin1-to-string (type-of object)) "local-time:timestamp")
        (progn
          (eval
           `(defmethod convert-for-sql ((object ,(type-of object)))
              (make-sql-variable
               (,(intern (string :format-timestring) :local-time)
                nil object
                :format '((:year 4) #\- (:month 2) #\- (:day 2)
                          #\Space
                          (:hour 2) #\: (:min 2) #\: (:sec 2) #\. (:usec 6))))))
          (convert-for-sql object))
        (make-sql-variable (princ-to-string object)))))

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

(defmethod yield ((raw splicing-raw-op))
  (values
   (format nil "~A"
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

(defmacro yield-for-union-ops (keyword)
  `(multiple-value-bind (statements others)
      (loop for obj in (conjunctive-op-expressions op)
            if (sql-statement-p obj)
              collecting obj into statements
            else
              collecting obj into others
            finally (return (values statements others)))
     (with-yield-binds
       (format nil (if *inside-select*
                       "(~{~A~^ ~})"
                       "~{~A~^ ~}")
               (append (list (format nil ,(format nil "~~{(~~A)~~^ ~a ~~}" keyword)
                                     (mapcar #'yield statements)))
                       (when others
                         (list (format nil "~{~A~^ ~}"
                                       (mapcar #'yield others)))))))))

(defmethod yield ((op union-op))
  (yield-for-union-ops "UNION"))

(defmethod yield ((op union-all-op))
  (yield-for-union-ops "UNION ALL"))

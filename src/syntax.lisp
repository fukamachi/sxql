(defpackage sxql.syntax
  (:use :cl)
  (:import-from :cl-annot)
  (:import-from :named-readtables
                :defreadtable
                :in-readtable)
  (:export :sxql-syntax
           :enable-syntax))
(in-package :sxql.syntax)

(defreadtable sxql-syntax
  (:merge :standard)
  (:macro-char #\@ #'cl-annot.syntax:annotation-syntax-reader))

(defmacro enable-syntax ()
  '(in-readtable sxql-syntax))

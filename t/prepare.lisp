(in-package :cl-user)
(defpackage t.sxql.prepare
  (:use :cl
        :cl-test-more)
  (:shadow :is-error)
  (:export :is-error))
(in-package :t.sxql.prepare)

(defmacro is-error (form condition &optional desc)
  #+ccl `(cl-test-more:is-error ,form ,condition ,desc)
  #-ccl
  (if (eq condition 'type-error)
      '(skip 1 "Testing type-error isn't supported")
      `(cl-test-more:is-error ,form ,condition ,desc)))

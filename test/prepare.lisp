(defpackage #:sxql/test/prepare
  (:nicknames #:t.sxql.prepare)
  (:use #:cl
        #:prove)
  (:shadow #:is-error)
  (:export #:is-error))
(in-package #:sxql/test/prepare)

(defmacro is-error (form condition &optional desc)
  (if #+sbcl (and (uiop:version< (lisp-implementation-version) "1.3.1")
                  (eq condition 'type-error))
      #+ccl nil
      #-(or sbcl ccl) (eq condition 'type-error)
      '(skip 1 "Testing type-error isn't supported")
      `(prove:is-error ,form ,condition ,desc)))

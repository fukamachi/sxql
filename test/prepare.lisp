(defpackage #:sxql/test/prepare
  (:nicknames #:t.sxql.prepare)
  (:use #:cl
        #:rove)
  (:shadow #:is-error)
  (:export #:is-error
           #:is-mv))
(in-package #:sxql/test/prepare)

(defmacro is-error (form condition &optional desc)
  "Test error conditions with platform-specific handling for Rove"
  (if #+sbcl (and (uiop:version< (lisp-implementation-version) "1.3.1")
                  (eq condition 'type-error))
      #+ccl nil
      #-(or sbcl ccl) (eq condition 'type-error)
      '(skip "Testing type-error isn't supported")
      (if (eq condition 'error)
          `(ok (signals ,form) ,desc)
          `(ok (signals ,form ',condition) ,desc))))

(defmacro is-mv (test result &optional desc)
  "Test multiple-value returns from yield function for Rove"
  `(ok (equal (multiple-value-list (sxql:yield ,test)) ,result) ,desc))

(in-package :cl-user)
(defpackage sxql.util
  (:use :cl
        :iterate)
  (:export :group-by
           :subdivide))
(in-package :sxql.util)

(defun group-by (key sequence &key (test 'eql))
  (let ((hash (make-hash-table :test test))
        (keys '()))
    (iter (for item in sequence)
      (push (funcall key item) keys)
      (push item (gethash (funcall key item) hash)))
    (iter (for key in (delete-duplicates (nreverse keys) :test test :from-end t))
      (collect key)
      (collect (nreverse (gethash key hash))))))

(defun subdivide (sequence chunk-size)
  "Split `sequence` into subsequences of size `chunk-size`."
  (check-type sequence sequence)
  (check-type chunk-size (integer 1))
  (etypecase sequence
    (list (loop :while sequence
                :collect
                (loop :repeat chunk-size
                      :while sequence
                      :collect (pop sequence))))
    (sequence (loop :with len := (length sequence)
                    :for i :below len :by chunk-size
                    :collect (subseq sequence i (min len (+ chunk-size i)))))))

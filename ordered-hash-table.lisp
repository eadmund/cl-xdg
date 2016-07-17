;;;; ordered-hash-table.lisp
;;;; Copyright (C) 2016  Robert A. Uhl

(in-package :cl-desktop)

(defclass ordered-hash-table ()
  ((hash :type hash-table
         :initarg :hash)
   (keys :type (vector t 1)
         :initarg :keys)))

(defun make-ordered-hash-table (&key
                                  (test 'eql)
                                  (size 16)
                                  (rehash-size 1.5)
                                  (rehash-threshold 1))
  (make-instance 'ordered-hash-table
                 :hash (make-hash-table :test test
                                        :size size
                                        :rehash-size rehash-size
                                        :rehash-threshold rehash-threshold)
                 :keys (make-array 1 :adjustable t :fill-pointer 0)))

(defun get-ordered-hash (key hash-table &optional default)
  (gethash key (slot-value hash-table 'hash) default))

(defun (setf get-ordered-hash) (value key hash-table)
  (with-slots (hash keys) hash-table
    (let* ((newsym (gensym))
           (old-value (gethash key hash newsym)))
      (when (eq old-value newsym)
        (vector-push-extend key keys)))
    (setf (gethash key hash) value)))

(defun rem-ordered-hash (key hash-table)
  (with-slots (hash keys) hash-table
    (when (remhash key hash)
      (let ((new-keys (remove key keys :test (hash-table-test hash))))
        (setf keys (make-array (length new-keys)
                               :adjustable t
                               :initial-contents new-keys
                               :fill-pointer (length new-keys)))))))

(defun map-ordered-hash (function hash-table)
  (with-slots (hash keys) hash-table
    (when (plusp (fill-pointer keys))
      (loop for i from 0 below (fill-pointer keys)
         for key = (aref keys i)
         do (funcall function key (gethash key hash))))))

;; maybe a WITH-ORDERED-HASH-TABLE-ITERATOR someday

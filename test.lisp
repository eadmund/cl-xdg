;;;; test.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(in-package "CL-USER")

(defpackage #:cl-xdg-test
  (:use #:cl #:fiveam #:cl-xdg))

(in-package #:cl-xdg-test)

(def-suite cl-xdg)
(in-suite cl-xdg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; FIXME: it'd be great were this like LET …
  (defmacro with-env ((var value) &body body)
  (let ((old-val (gensym "OLD-VALUE")))
    `(let ((,old-val (uiop:getenv ,var)))
       (unwind-protect
            (progn
              (setf (uiop:getenv ,var) ,value)
              ,@body)
         (setf (uiop:getenv ,var) ,old-val))))))

(test load-desktop-files
  (with-env ("XDG_DATA_HOME" (namestring (asdf:system-relative-pathname '#:cl-xdg #P"test/")))
    (let ((files (load-desktop-files)))
      (is-true files)
      (is-true (cl-xdg::find-desktop-file-by-id files "foo.desktop")))))

(test desktop-file
  (with-env ("XDG_DATA_HOME" (namestring (asdf:system-relative-pathname '#:cl-xdg #P"test/")))
    (let* ((files (load-desktop-files))
           (file (cl-xdg::find-desktop-file-by-id files "foo.desktop")))
      (is (null (get-string-key "Does-not-exist" file)))
      (is (string= (get-string-key "Name" file) "Test"))
      (is (string= (get-string-key "Field2" file) "Test2"))
      (is (string= (get-locale-string-key "Field2" file :locales '("en")) "Test3"))
      (is (string= (get-locale-string-key "Field2" file :locales '("en@test")) "Test4"))
      (is (string= (get-locale-string-key "Field2" file :locales '("en_US")) "Test5"))
      (is (string= (get-locale-string-key "Field2" file :locales '("en_US@test")) "Test6"))
      (is (string= (get-locale-string-key "Field2" file :locales '("ang")) "Hwæt"))
      (is (= (get-number-key "Field3" file) 12))
      (is-true (get-boolean-key "Field4" file)))))

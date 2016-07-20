;;;; test.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(in-package "CL-USER")

(defpackage #:cl-xdg-test
  (:use #:cl #:fiveam #:cl-xdg))

(in-package #:cl-xdg-test)

(def-suite cl-xdg)
(in-suite cl-xdg)

(test load-desktop-files
  (let ((files (load-desktop-files)))
    (is-true files)))

(run!)

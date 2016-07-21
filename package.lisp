;;;; package.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(in-package "CL-USER")

(defpackage #:cl-xdg
  (:use #:cl)
  (:export #:load-desktop-file
           #:load-desktop-files
           #:find-desktop-file-by-id

           #:desktop-file
           #:desktop-files

           #:id
           #:path
           #:get-string-key
           #:get-strings-key
           #:get-locale-string-key
           #:get-locale-strings-key
           #:get-boolean-key
           #:get-number-key))

;;;; package.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(defpackage #:cl-xdg
  (:use #:cl)
  (:export #:parse-desktop-file
           #:load-desktop-files

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

;;;; package.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(defpackage #:cl-desktop
  (:use #:cl)
  (:export #:parse-desktop-file
           #:get-string-key
           #:get-strings-key
           #:get-locale-string-key
           #:get-locale-strings-key
           #:get-boolean-key
           #:get-number-key
           #:*xdg-data-home*
           #:*xdg-config-home*
           #:*xdg-data-dirs*
           #:*xdg-config-dirs*
           #:*xdg-cache-home*
           #:*xdg-runtime-dir*
           #:load-desktop-files))

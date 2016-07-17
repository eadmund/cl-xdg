;;;; package.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(defpackage #:cl-desktop
  (:use #:cl)
  (:export #:parse-desktop-file
           #:get-key
           #:*xdg-data-home*
           #:*xdg-config-home*
           #:*xdg-data-dirs*
           #:*xdg-config-dirs*
           #:*xdg-cache-home*
           #:*xdg-runtime-dir*
           #:load-desktop-files))

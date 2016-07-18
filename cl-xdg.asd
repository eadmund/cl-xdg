;;;; cl-xdg.asd
;;;; Copyright (C) 2016 by Robert A. Uhl

(asdf:defsystem #:cl-xdg
  :description "freedesktop.org standards handling"
  :author "Bob Uhl <bob.denver.co@gmail.com>"
  :license "GNU General Public License"
  :serial t
  :depends-on (:uiop :split-sequence :parse-number)
  :components ((:file "package")
               (:file "xdg-vars")
               (:file "ordered-hash-table")
               (:file "desktop")))

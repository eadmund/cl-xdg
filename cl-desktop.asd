;;;; cl-desktop.asd
;;;; Copyright (C) 2016 by Robert A. Uhl

(asdf:defsystem #:cl-desktop
  :description "freedesktop.org desktop entry handling"
  :author "Bob Uhl <bob.denver.co@gmail.com>"
  :license "GNU General Public License"
  :serial t
  :components ((:file "package")
               (:file "cl-desktop")))

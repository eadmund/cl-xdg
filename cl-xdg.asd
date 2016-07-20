;;;; cl-xdg.asd
;;;; Copyright (C) 2016 by Robert A. Uhl

(asdf:defsystem #:cl-xdg
  :description "freedesktop.org standards handling"
  :author "Bob Uhl <bob.denver.co@gmail.com>"
  :license "GNU General Public License"
  :serial t
  :depends-on (#:uiop #:split-sequence #:parse-number #+sbcl #:sb-posix)
  :in-order-to ((test-op (test-op #:cl-xdg-test)))
  :components ((:file "package")
               (:file "ordered-hash-table")
               (:file "desktop")))

(defsystem #:cl-xdg-test
  :depends-on (#:cl-xdg #:fiveam)
  :components ((:file "test"))
  :perform (test-op (o s)
                    (uiop:symbol-call '#:fiveam '#:run!)))

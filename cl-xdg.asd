;;;; cl-xdg.asd
;;;; Copyright (C) 2016 by Robert A. Uhl

(asdf:defsystem #:cl-xdg
  :description "freedesktop.org standards handling"
  :author "Bob Uhl <bob.denver.co@gmail.com>"
  :license "GNU General Public License"
  :serial t
  :depends-on (#:uiop
               #:split-sequence
               #:parse-number
               #:flexi-streams
               #:cl-sxml
               #:cl-xmlspam
               #+sbcl #:sb-posix )
  :in-order-to ((test-op (test-op #:cl-xdg-test)))
  :components ((:file "package")
               (:file "ordered-hash-table")
               (:file "desktop")
               (:file "menus")))

(defsystem #:cl-xdg-test
  :depends-on (#:cl-xdg #:fiveam #:asdf #:uiop)
  :components ((:file "package")
               (:file "test"))
  :perform (test-op (o s)
                    (let ((results
                           (uiop:symbol-call '#:fiveam
                                             '#:run
                                             (uiop:find-symbol* '#:cl-xdg
                                                                '#:cl-xdg-test))))
                      (unless (uiop:symbol-call '#:fiveam '#:results-status
                                                results)
                        (uiop:symbol-call :fiveam '#:explain! results)
                        (warn "tests failed")))))

;;;; test.lisp
;;;; Copyright (C) 2016 by Robert A. Uhl

(in-package "CL-USER")

(defpackage #:cl-xdg-test
  (:use #:cl #:fiveam #:cl-xdg))

(in-package #:cl-xdg-test)

(def-suite cl-xdg)
(in-suite cl-xdg)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun canonicalise-env-binding (binding)
    ;; user bindings may be of the form "VAR", VAR, ("VAR" value) or (VAR value)
    (etypecase binding
      (list
       (etypecase (first binding)
         (symbol (setf (first binding) (symbol-name (first binding))))
         (string))
       (push (gensym (concatenate 'string "OLD-" (first binding))) binding))
      (string (list (gensym (concatenate 'string "OLD-" binding)) binding ""))
      (symbol (list (gensym (concatenate 'string "OLD-" (symbol-name binding)))
                    (symbol-name binding)
                    ""))))

  (defmacro with-env (bindings &body body)
    "Set environment variables to values according to BINDINGS, then
execute BODY, the revert the environment variable settings.

Bindings may be of the form VARIABLE (a symbol),
\"VARIABLE\", (VARIABLE VALUE) or (\"VARIABLE\" value).  Values must
evaluate to strings.

BUG: cannot unset an environment variable, but rather sets it to \"\"."
    (loop for binding in (mapcar #'canonicalise-env-binding bindings)
       with return-value = (gensym "RETURN-VALUE")
       collect `(,(first binding) (uiop:getenvp ,(second binding))) into let-bindings
       collect `(setf (uiop:getenv ,(second binding)) ,(third binding)) into setf-bindings
       collect `(setf (uiop:getenv ,(second binding)) (or ,(first binding) "")) into unsetf-bindings
       finally (return `(let (,return-value ,@let-bindings)
                          ,@setf-bindings
                          (setf ,return-value (progn ,@body))
                          ,@unsetf-bindings
                          ,return-value)))))

(test load-desktop-files
  (with-env (("XDG_DATA_HOME"
              (namestring (asdf:system-relative-pathname '#:cl-xdg #P"test/")))
             ("XDG_DATA_DIRS" "/PATH/to/NOWHERE/7c1cef10-d57d-466d-9efc-f97110db112d/"))
    (let ((files (load-desktop-files)))
      (is-true files)
      (is (= (hash-table-count (slot-value files 'cl-xdg::files)) 1))
      (is-true (cl-xdg::find-desktop-file-by-id files "foo.desktop")))))

(test desktop-file
  (with-env (("XDG_DATA_HOME"
              (namestring (asdf:system-relative-pathname '#:cl-xdg #P"test/")))
             ("XDG_DATA_DIRS" "/path/TO/nowhere/6d1a6023-c194-4a70-a3c2-0e0c19df2bac/"))
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

(run!)

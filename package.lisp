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

(defpackage #:cl-xdg/sxml
  (:use)
  (:export #:|Menu|
           #:|Name|
           #:|Directory|
           #:|DefaultAppDirs|
           #:|AppDir|
           #:|DefaultDirectoryDirs|
           #:|DirectoryDir|
           #:|LegacyDir|
           #:|KDELegacyDirs|
           #:|MergeFile|
           #:|DefaultMergeDirs|
           #:|MergeDir|
           #:|OnlyUnallocated|
           #:|NotOnlyUnallocated|
           #:|Deleted|
           #:|NotDeleted|
           #:|Include|
           #:|Exclude|
           #:|Move|
           #:|Menu|
           #:|Layout|
           #:|DefaultLayout|
           #:|prefix|
           #:|type|
           #:|Category|
           #:|Filename|
           #:|And|
           #:|Or|
           #:|Not|
           #:|All|
           #:|Old|
           #:|New|
           #:|Menuname|
           #:|Separator|
           #:|Merge|
           #:|show_empty|
           #:|inline|
           #:|inline_limit|
           #:|inline_header|
           #:|inline_alias|
           #:*top*
           #:@
           #:*doctype*
           #:*pi*))

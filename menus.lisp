;;;; menus.lisp
;;;; Copyright (C) 2016  Robert A. Uhl

(in-package #:cl-xdg)

(defun dont-resolve-entities (a b)
  (declare (ignore a b))
  (flexi-streams:make-in-memory-input-stream nil))

(defun build-filter (source)
  (xspam:with-xspam-source source
    (let (exprs)
      (xspam:zero-or-more
       (xspam:one-of
        (xspam:element "And"
          (push (list 'and (build-filter source)) exprs))
        (xspam:element "Or"
          (push (list 'or (build-filter source)) exprs))
        (xspam:element "Not"
          (push (list 'not (build-filter source)) exprs))
        (xspam:element "All"
          (return t))
        (xspam:element "Filename"
          (xspam:text
           (push (list 'filename xspam:_) exprs)))
        (xspam:element "Category"
          (xspam:text
           (push (list 'category xspam:_) exprs)))))
      (if (> (length exprs) 1)
          (cons 'or exprs)
          (first exprs)))))

(defun merge-menus (a b)
  "Merge B into A.  A should be a menu plist being processed; B should
  be a completed menu plist.

1. Merge child menus
2. Resolve duplicate Move elements"
  (flet ((extract-submenus (menu hash start)
           (loop for submenu in (getf menu :submenus)
              for i from start
              for pre-existing = (gethash (getf submenu :name) hash)
              do (setf (gethash (getf submenu :name) hash)
                       (cons i (if pre-existing
                                   (merge-menus (cdr pre-existing) submenu)
                                   submenu)))
              finally (return i))))
    (let* ((hash (make-hash-table :test 'equal))
           submenus
           (app-dirs (delete-duplicates (append (getf a :app-dirs)
                                                (getf b :app-dirs))
                                        :from-end t
                                        :test #'equal))
           (directory-dirs (delete-duplicates (append (getf a :directory-dirs)
                                                      (getf b :directory-dirs))
                                              :from-end t
                                              :test #'equal))
           (filter-a (getf a :filter))
           (filter-b (getf b :filter))
           (filter (cond
                     ((and filter-a filter-b) (list filter-a filter-b))
                     (filter-a filter-a)
                     (filter-b filter-b))))
      (extract-submenus b hash (extract-submenus a hash 0))
      (maphash (lambda (k v)
                 (declare (ignorable k))
                 (push v submenus))
               hash)
      (setf submenus (mapcar #'cdr (sort submenus #'< :key #'car)))
      `(:name ,(getf a :name)
              ,@(when app-dirs `(:app-dirs ,app-dirs))
              ,@(when directory-dirs `(:directory-dirs ,directory-dirs))
              ,@(when filter `(:filter ,filter))
              ,@(when submenus `(:submenus ,submenus))))))

(defvar *menu-files-in-flight* nil)

(defun read-menu (source)
  (xspam:with-xspam-source source
    (let (name
          submenus
          app-dirs
          directory-dirs
          directories
          only-unallocated
          deleted
          filter)
      (xspam:zero-or-more
       (xspam:one-of
        (xspam:element "Name"
          (xspam:text
           (when name
             (error "more than one name in menu (have ~a; got ~a)"
                    name xspam:_))
           (setf name xspam:_)))
        (xspam:element "Menu"
          (push (read-menu source) submenus))
        (xspam:element "AppDir"
          (xspam:text (push xspam:_ app-dirs)))
        (xspam:element "DefaultAppDirs"
          (loop for dir in (reverse (cons (uiop:xdg-data-home)
                                          (uiop:xdg-data-dirs)))
             do (push (uiop:subpathname* dir #P"applications/") app-dirs)))
        (xspam:element "DirectoryDir"
          (xspam:text (push xspam:_ directory-dirs)))
        (xspam:element "DefaultDirectoryDirs"
          (loop for dir in (reverse (cons (uiop:xdg-data-home)
                                          (uiop:xdg-data-dirs)))
             do (push (uiop:subpathname* dir #P"desktop-directories/")
                      directory-dirs)))
        (xspam:element "Directory"
          (xspam:text (push xspam:_ directories)))
        (xspam:element "OnlyUnallocated"
          (setf only-unallocated t))
        (xspam:element "NotOnlyUnallocated"
          (setf only-unallocated nil))
        (xspam:element "Deleted"
          (setf deleted t))
        (xspam:element "NotDeleted"
          (setf deleted nil))
        (xspam:element "Include"
          ;;(format t "~&inc ~a" (build-filter source)))
          (push (cons 'include (build-filter source)) filter))
        (xspam:element "Exclude"
          (push (cons 'exclude (build-filter source)) filter))
        (xspam:element "MergeFile"
         (let ((type 'path)
               path)
           (xspam:optional-attribute "type"
             (setf type
                   (cond
                     ((string= xspam:_ "path") 'path)
                     ((string= xspam:_ "parent") 'parent)
                     (t (error "Unknown MergeFile type ~a" xspam:_)))))
           (xspam:optional
            (xspam:text
             (when (eq type 'path)
               (setf path (pathname xspam:_)))))
           (when (find path *menu-files-in-flight* :test 'equal)
             ;; FIXME: in the future, offer ignorable restart
             (error "infinite menu merge detected"))
           (push path *menu-files-in-flight*)
           (destructuring-bind (&key child-name
                                     child-app-dirs
                                     child-directory-dirs
                                     child-directories
                                     (child-only-unallocated
                                      nil
                                      only-unallocated-p)
                                     (child-deleted nil deleted-p)
                                     child-filter)
               (read-menu (xspam:make-xspam-source
                           path
                           :entity-resolver #'dont-resolve-entities))
             (declare (ignore child-name))
             (when child-app-dirs
               (setf app-dirs
                     (delete-duplicates (append child-app-dirs app-dirs)
                                        :from-end t)))
             (when child-directory-dirs
               (setf directory-dirs
                     (delete-duplicates (append child-directory-dirs
                                                directory-dirs)
                                        :from-end t)))
             (when child-directories
               (setf directories
                     (delete-duplicates (append child-directories
                                                directories)
                                        :from-end t)))
             (when only-unallocated-p
               (setf only-unallocated child-only-unallocated))
             (when deleted-p
               (setf deleted child-deleted))
             (setf filter
                   (cond
                     ((and filter child-filter) `(or filter child-filter))
                     (filter filter)
                     (child-filter child-filter))))))))
      `(:name ,(or name (error "no name for menu"))
              ,@(when submenus `(:submenus ,(nreverse submenus)))
              ,@(when app-dirs `(:app-dirs ,app-dirs))
              ,@(when directory-dirs `(:directory-dirs ,directory-dirs))
              ,@(when directories `(:directories ,directories))
              ,@(when only-unallocated `(:only-unallocated ,only-unallocated))
              ,@(when deleted `(:deleted ,deleted))
              ,@(when filter
                  `(:filter ,filter))))))

(defun load-menu-file (filespec)
  (when (find filespec *menu-files-in-flight* :test 'equal)
    (return-from load-menu-file nil))
  (let ((source (xspam:make-xspam-source
                 (pathname filespec)
                 :entity-resolver #'dont-resolve-entities))
        (*menu-files-in-flight* (cons filespec *menu-files-in-flight*)))
    (xspam:with-xspam-source source
      ;; FIXME: after reading, deduplicate & consolidate
      (xspam:element "Menu" (read-menu source)))))

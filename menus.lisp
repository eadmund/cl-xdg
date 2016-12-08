;;;; menus.lisp
;;;; Copyright (C) 2016  Robert A. Uhl

(in-package #:cl-xdg)

(defun dont-resolve-entities (a b)
  (declare (ignore a b))
  (flexi-streams:make-in-memory-input-stream nil))

(defun dtd-resolver (public-id system-id)
  (declare (ignore system-id))
  (cond
    ((find public-id '("-//freedesktop//DTD Menu 1.0//EN"
                       "-//freedesktop//DTD Menu 0.8//EN")
           :test #'string=)
     (open (asdf:system-relative-pathname '#:cl-xdg "dtds/menu-latest.dtd")))))

;; (defun build-filter (source)
;;   (xspam:with-xspam-source source
;;     (let (exprs)
;;       (xspam:zero-or-more
;;        (xspam:one-of
;;         (xspam:element "And"
;;           (push (list 'and (build-filter source)) exprs))
;;         (xspam:element "Or"
;;           (push (list 'or (build-filter source)) exprs))
;;         (xspam:element "Not"
;;           (push (list 'not (build-filter source)) exprs))
;;         (xspam:element "All"
;;           (return t))
;;         (xspam:element "Filename"
;;           (xspam:text
;;            (push (list 'filename xspam:_) exprs)))
;;         (xspam:element "Category"
;;           (xspam:text
;;            (push (list 'category xspam:_) exprs)))))
;;       (if (> (length exprs) 1)
;;           (cons 'or exprs)
;;           (first exprs)))))

;; (defun merge-menus (a b)
;;   "Merge B into A.  A should be a menu plist being processed; B should
;;   be a completed menu plist.

;; 1. Merge child menus
;; 2. Resolve duplicate Move elements"
;;   (flet ((extract-submenus (menu hash start)
;;            (loop for submenu in (getf menu :submenus)
;;               for i from start
;;               for pre-existing = (gethash (getf submenu :name) hash)
;;               do (setf (gethash (getf submenu :name) hash)
;;                        (cons i (if pre-existing
;;                                    (merge-menus (cdr pre-existing) submenu)
;;                                    submenu)))
;;               finally (return i))))
;;     (let* ((hash (make-hash-table :test 'equal))
;;            submenus
;;            (app-dirs (delete-duplicates (append (getf a :app-dirs)
;;                                                 (getf b :app-dirs))
;;                                         :from-end t
;;                                         :test #'equal))
;;            (directory-dirs (delete-duplicates (append (getf a :directory-dirs)
;;                                                       (getf b :directory-dirs))
;;                                               :from-end t
;;                                               :test #'equal))
;;            (filter-a (getf a :filter))
;;            (filter-b (getf b :filter))
;;            (filter (cond
;;                      ((and filter-a filter-b) (list filter-a filter-b))
;;                      (filter-a filter-a)
;;                      (filter-b filter-b))))
;;       (extract-submenus b hash (extract-submenus a hash 0))
;;       (maphash (lambda (k v)
;;                  (declare (ignorable k))
;;                  (push v submenus))
;;                hash)
;;       (setf submenus (mapcar #'cdr (sort submenus #'< :key #'car)))
;;       `(:name ,(getf a :name)
;;               ,@(when app-dirs `(:app-dirs ,app-dirs))
;;               ,@(when directory-dirs `(:directory-dirs ,directory-dirs))
;;               ,@(when filter `(:filter ,filter))
;;               ,@(when submenus `(:submenus ,submenus))))))

(defvar *menu-files-in-flight* nil)

(defun read-menu (filespec)
  (let* ((handler (make-instance 'cl-sxml:sxml-handler
                                 :package (find-package '#:cl-xdg/sxml)))
         (truename (uiop:ensure-pathname filespec))
         (*menu-files-in-flight* (cons truename *menu-files-in-flight*)))
    (unless truename
      (error "cannot find menu file ~a" filespec))
    (when (find truename (rest *menu-files-in-flight*) :test #'equal)
      (error "already loading menu file ~a" filespec))
    (car (merge-menus
          (cxml:parse-file truename handler :entity-resolver #'dont-resolve-entities)))))

(defun merge-path (path)
  (rest
   (remove 'cl-xdg/sxml:|Name|
           (find 'cl-xdg/sxml:|Menu|
                 (cdar
                  (merge-menus
                   (read-menu
                    (uiop:merge-pathnames* path (first *menu-files-in-flight*)))))
                 :key #'first)
           :key (lambda (x) (when (consp x) (first x))))))

(defun merge-parent ()
  ;; see if the current file is a child of any XDG data directory
  (let ((file (first *menu-files-in-flight*))
        (data-dirs (cons (uiop:xdg-data-home) (uiop:xdg-data-dirs))))
    (loop for dir in data-dirs
       for path = (uiop:subpathp file dir)
       when path
       do (loop for parent-dir in (remove dir data-dirs :test #'equal)
             for subpath = (uiop:file-exists-p
                            (uiop:merge-pathnames* path parent-dir))
             when subpath
             do (return-from merge-parent (merge-path subpath))))))

(defun merge-file (element)
  (cond
    ((and (consp (second element))
           (eq (first (second element)) 'cl-xdg/sxml:@))
     (let ((type (second
                  (find 'cl-xdg/sxml:|type| (rest (second element)) :key #'first))))
       (cond
         ((string= type "parent") (merge-parent))
         ((string= type "path") (merge-path (third element)))
         (t (error "don't understand MergeFile type ~a" type)))))
    ((stringp (second element))
     (merge-path (second element)))
    (t (error "unparseable MergeFile contents: ~s" (second element)))))

(defun merge-dir (path)
  (let* ((path (uiop:ensure-directory-pathname path))
         (dir (uiop:subpathname
               (uiop:pathname-directory-pathname(first *menu-files-in-flight*))
               path))
         (paths (uiop:directory* (uiop:merge-pathnames* #P"*.menu" dir))))
    (append (mapcar #'merge-path paths))))

(defun merge-menus (element)
  "Process MergeFile, MergeDir and LegacyDir elements, and remove
extraneous whitespace."
  (typecase element
    (string (let ((string (string-trim
                           '(#\Space #\Backspace #\Tab #\Linefeed #\Page
                             #\Return #\Newline #\Rubout)
                           element)))
              (when (string/= string "")
                (list string))))
    (symbol (list element))
    (cons (case (car element)
            (cl-xdg/sxml:|MergeFile| (merge-file element))
            (cl-xdg/sxml:|MergeDir| (merge-dir (second element)))
            (cl-xdg/sxml:|LegacyDir| )
            (t (list (apply 'concatenate 'cons (mapcar #'merge-menus element))))))))

#|(defun merge-if (element)
  (when ))

(defun fold-menu (menu)
  (mapl ))

(defun read-menu (path source)
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
          (push (read-menu path source) submenus))
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
               child-path)
           (xspam:optional-attribute "type"
             (setf type
                   (cond
                     ((string= xspam:_ "path") 'path)
                     ((string= xspam:_ "parent") 'parent)
                     (t (error "Unknown MergeFile type ~a" xspam:_)))))
           (xspam:optional
            (xspam:text
             (when (eq type 'path)
               (setf child-path (pathname xspam:_)))))
           (format t "~&~a ~a ~a" type child-path (uiop:relative-pathname-p child-path))
           (when (and (eq type 'path) (uiop:relative-pathname-p child-path))
             (setf child-path (uiop:merge-pathnames* child-path path)))
           (format t "~&~a ~a" child-path (uiop:relative-pathname-p child-path))
           (when (find child-path *menu-files-in-flight* :test 'equal)
             ;; FIXME: in the future, offer ignorable restart
             (error "infinite menu merge detected"))
           (push child-path *menu-files-in-flight*)
           (destructuring-bind (&key child-name
                                     child-app-dirs
                                     child-directory-dirs
                                     child-directories
                                     (child-only-unallocated
                                      nil
                                      only-unallocated-p)
                                     (child-deleted nil deleted-p)
                                     child-filter)
               (read-menu child-path
                          (xspam:make-xspam-source
                           child-path
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
  (let* ((path (pathname filespec))
         (source (xspam:make-xspam-source
                 path
                 :entity-resolver #'dont-resolve-entities))
        (*menu-files-in-flight* (cons filespec *menu-files-in-flight*)))
    (xspam:with-xspam-source source
      ;; FIXME: after reading, deduplicate & consolidate
      (xspam:element "Menu" (read-menu path source)))))
|#

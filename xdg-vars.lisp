;;;; xdg-vars.lisp
;;;; Copyright (C) 2016  Robert A. Uhl
;; taken from StumpWM; copied under terms of the GPL

(in-package #:cl-xdg)

(defun getenv (var &optional default)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (ext:getenv (string var))
  #+(or cmu scl)
  (cdr (assoc (string var) ext:*environment-list* :test #'equalp
              :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+mcl (ccl::getenv var)
  #+sbcl (sb-posix:getenv (string var))
  #+openmcl (ccl:getenv (string var))
  #+ecl (ext:getenv (string var))
  #-(or allegro clisp cmu gcl lispworks lucid mcl sbcl scl openmcl
        ecl)
  (error 'not-implemented))

;; taken from StumpWM; copied under terms of the GPL
(defun (setf getenv) (val var)
  "Set the value of the environment variable, @var{var} to @var{val}."
  #+allegro (setf (sys::getenv (string var)) (string val))
  #+clisp (setf (ext:getenv (string var)) (string val))
  #+(or cmu scl)
  (let ((cell (assoc (string var) ext:*environment-list* :test #'equalp
                     :key #'string)))
    (if cell
        (setf (cdr cell) (string val))
        (push (cons (intern (string var) "KEYWORD") (string val))
              ext:*environment-list*)))
  #+gcl (si:setenv (string var) (string val))
  #+lispworks (setf (lw:environment-variable (string var)) (string val))
  #+lucid (setf (lcl:environment-variable (string var)) (string val))
  #+sbcl (sb-posix:putenv (format nil "~A=~A" (string var) (string val)))
  #+openmcl (ccl:setenv (string var) (string val))
  #+ecl (ext:setenv (string var) (string val))
  #-(or allegro clisp cmu gcl lispworks lucid sbcl scl openmcl ecl)
  (error 'not-implemented))

(defmacro getenv-or-default (var default)
  (let ((val (gensym "VAL")))
    `(let ((,val (getenv ,var)))
       (if (and ,val (string/= ,val ""))
           ,val
           ,default))))

(defvar *xdg-data-home*
  (uiop:ensure-directory-pathname
   (getenv-or-default "XDG_DATA_HOME"
                      (merge-pathnames ".local/share/" (user-homedir-pathname))))
  "Base directory relative to which user-specific data files should be stored.")

(defvar *xdg-config-home*
  (uiop:ensure-directory-pathname
   (getenv-or-default "XDG_CONFIG_HOME"
                      (merge-pathnames ".config/" (user-homedir-pathname))))
  "Base directory relative to which user-specific configuration files
  should be stored.")

(defvar *xdg-data-dirs*
  (mapcar #'uiop:ensure-directory-pathname
          (split-sequence:split-sequence
           #\:
           (getenv-or-default "XDG_DATA_DIRS" "/usr/local/share/:/usr/share/")))
  "Preference-ordered list of base directories to search for data
  files in addition to *xdg-data-home*.")

(defvar *xdg-config-dirs*
  (mapcar #'uiop:ensure-directory-pathname
          (split-sequence:split-sequence
           #\:
           (getenv-or-default "XDG_CONFIG_DIRS" "/etc/xdg/")))
  "Preference-ordered list of base directories to search for
  configuration files in addition to *xdg-config-home*.")

(defvar *xdg-cache-home*
  (uiop:ensure-directory-pathname
   (getenv-or-default "XDG_CACHE_HOME"
                      (merge-pathnames ".cache/" (user-homedir-pathname))))
  "Base directory relative to which user-specific non-essential data
  files should be stored.")

(defvar *xdg-runtime-dir*
  (uiop:ensure-directory-pathname
   (getenv-or-default "XDG_RUNTIME_DIR"
                      #+sbcl(sb-posix:mkdtemp "/tmp/branch.XXXXXX")
                      #-sbcl(progn (warn "XDG_RUNTIME_DIR not set")
                                   nil)))
  "Base directory relative to which user-specific non-essential
  runtime files should be stored.")

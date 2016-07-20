;;;; xdg-vars.lisp
;;;; Copyright (C) 2016  Robert A. Uhl
;; taken from StumpWM; copied under terms of the GPL

(in-package #:cl-xdg)

(defmacro getenv-or-default (var default)
  `(or (uiop:getenvp ,var) ,default))

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

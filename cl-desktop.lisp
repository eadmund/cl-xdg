;;;; cl-desktop.lisp

;;;; cl-desktop - freedesktop.org desktop file handling
;;;; Copyright (C) 2016  Robert A. Uhl
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:cl-desktop)

;; taken from StumpWM; copied under terms of the GPL
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
                      #-sbcl(error "XDG_RUNTIME_DIR not set")))
  "Base directory relative to which user-specific non-essential data
  files should be stored.")

;; Right now, files are just parsed to a fairly dumb list, and
;; searched with a linear search.  It'd be nice to have a smarter
;; representation, with less linear searching, but that can wait.
;; This representation does make it quite easy to write out files with
;; modifications while preserving their order.

(defun parse-desktop-file (filespec)
  "Parse a desktop file."
  (with-open-file (file filespec :external-format :utf-8)
    (loop for line = (read-line file nil :eof)
       until (eq line :eof)
       collect (parse-desktop-file-line line))))

(defun parse-desktop-file-line (line)
  (cond
    ((or (string= line "") (char= (aref line 0) #\#)) line)
    ((char= (aref line 0) #\[)
     (if (char= (aref line (1- (length line))) #\])
         `(:group ,(subseq line 1 (1- (length line))))
         (error "invalid group in line ~a" line)))
    (t
     (let ((split (position #\= line)))
       (unless split
         (error "invalid line ~a" line))
       `(,(subseq line 0 split) . ,(subseq line (1+ split)))))))

(defun get-key (key file &key (group "Desktop Entry"))
  (loop for line in file
     with in-group
     when (and (consp line) (eq (first line) :group))
     do (setf in-group (string= (second line) group))
     when (and in-group (consp line) (string= (car line) key))
     return (cdr line)))

(defun load-desktop-files (&optional (subdir #P"applications/"))
  "Load desktop files from SUBDIR underneath *XDG-DATA-HOME* and each
  of *XDG-DATA-DIRS*.  Desktop files found under #P\"applications/\"
  have IDs; files earlier in the search path take precedence over
  files later in the search path with the same ID."
  (mapcar #'parse-desktop-file
          (apply #'concatenate 'list
                 (mapcar
                  (lambda (dir)
                    (directory
                     (uiop:merge-pathnames*
                      "*.desktop"
                      (uiop:wilden (uiop:merge-pathnames* subdir dir)))))
                  (cons *xdg-data-home* *xdg-data-dirs*)))))

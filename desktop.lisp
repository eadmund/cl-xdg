;;;; cl-xdg.lisp

;;;; cl-xdg - freedesktop.org desktop file handling
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

(in-package #:cl-xdg)

(defclass desktop-files ()
  ((files :type hash-table
          :initarg :files)))

(defclass desktop-file ()
  ((hash :type ordered-hash-table
         :initarg :hash)
   (id :type string
       :initarg :id
       :reader id)
   (path :type pathname
         :initarg :path
         :reader path)))

(defun load-desktop-file (filespec)
  "Load the desktop file found in FILESPEC into a DESKTOP-FILE object."
  (with-open-file (file filespec :external-format asdf:*utf-8-external-format*)
    ;; stash each line of the file into an ordered hash table, with a
    ;; key designed to be easy to look up: comments just get a gensym;
    ;; group lines use the group name; keys use (GROUP KEY) or (GROUP
    ;; KEY LOCALE).
    (loop with group = nil
       with hash = (make-ordered-hash-table :test 'equal)
       for line = (read-line file nil :eof)
       until (eq line :eof)
       do (destructuring-bind (key value)
              (parse-desktop-file-line line group)
            (when (stringp key)
              (setf group key))
            (setf (get-ordered-hash key hash) value))
       finally (return (make-instance 'desktop-file
                                      :hash hash
                                      :path (pathname filespec))))))

(defun parse-desktop-file-line (line current-group)
  "Given a line and the currently-active group, return a key and a
  value to store in the desktop file ordered hash.  The possibilities
  are:

 - comment: (GENSYM COMMENT)
 - group: (GROUP NIL)
 - key: ((GROUP KEY LOCALE) VALUE) or ((GROUP KEY) VALUE)"
  (cond
    ;; comment
    ((or (string= line "") (char= (aref line 0) #\#))
     (list (gensym "COMMENT") line))
    ;; group
    ((char= (aref line 0) #\[)
     (if (char= (aref line (1- (length line))) #\])
         (list (subseq line 1 (1- (length line))) nil)
         (error "invalid group in line ~a" line)))
    ;; key
    (t
     (when (null current-group)
       (error "key found without an active group"))
     (let* ((pos (position #\= line))
            (key (subseq line 0 pos))
            (value (subseq line (1+ pos))))
       (if (char= (aref key (1- (length key))) #\])
           (destructuring-bind (key locale)
               (split-sequence:split-sequence #\[ key :end (1- (length key)))
             `((,current-group ,key ,locale) ,value))
           `((,current-group ,key) ,value))))))

(defun lc-messages-to-locales ()
  "Convert LC_MESSAGES to a preference-ordered list of locales."
  (let ((lc-messages (uiop:getenvp "LC_MESSAGES")))
    (when lc-messages
      (destructuring-bind (lang country encoding modifier &aux locales)
          (parse-locale lc-messages)
        (declare (ignore encoding))
        (when (and lang country modifier)
          (push (format nil "~a_~a@~a" lang country modifier) locales))
        (when (and lang country)
          (push (format nil "~a_~a" lang country) locales))
        (when (and lang modifier)
          (push (format nil "~a@~a" lang modifier) locales))
        (when lang (push lang locales))
        (reverse locales)))))

(defun parse-locale (locale)
  (loop for char across locale and i from 0
         with lang = 0 and country and encoding and modifier
         do (case char
              (#\_ (cond
                     ((integerp lang) (setf lang (subseq locale lang i)))
                     (t (error "Unexpected #\_.")))
                   (setf country (1+ i)))
              (#\. (cond
                     ((integerp lang) (setf lang (subseq locale lang i)))
                     ((integerp country) (setf country (subseq locale country i)))
                     (t (error "Unexpected #\..")))
                   (setf encoding (1+ i)))
              (#\@ (cond
                     ((integerp lang) (setf lang (subseq locale lang i)))
                     ((integerp country) (setf country (subseq locale country i)))
                     ((integerp encoding) (setf encoding (subseq locale encoding i)))
                     (t (error "Unexpected #\@.")))
                   (setf modifier (1+ i))))
         finally (progn
                   (cond
                     ((integerp lang) (setf lang (subseq locale lang)))
                     ((integerp country) (setf country (subseq locale country)))
                     ((integerp encoding) (setf encoding (subseq locale encoding)))
                     ((integerp modifier) (setf modifier (subseq locale modifier))))
                   (return (list lang country encoding modifier)))))

;; taken from http://cl-cookbook.sourceforge.net/strings.html
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(defun replace-escapes (str)
  (let ((escapes '(("\\s" . " ")
                   ("\\n" . "
")
                   ("\\t" . "	")
                   ("\\r" . "")
                   ("\\\\" . "\\"))))
    (loop for (escape . replacement) in escapes
       do (setf str (replace-all str escape replacement)))
    str))

(defun get-string-key (key file &key (group "Desktop Entry"))
  (let ((str (get-ordered-hash `(,group ,key) (slot-value file 'hash))))
    (when str
      (replace-escapes str))))

(defun split-multi-string (str)
  (loop for char across str
     with items and acc and in-escape
     do (case char
          (#\\ (if in-escape
                   (push #\\ acc)
                   (setf in-escape t)))
          (#\s (push (if in-escape
                         #\Space
                         #\s)
                     acc))
          (#\n (push (if in-escape
                         #\Newline
                         #\n)
                     acc))
          (#\t (push (if in-escape
                         #\Tab
                         #\t)
                     acc))
          (#\r (push (if in-escape
                         #\Return
                         #\r)
                     acc))
          (#\; (if in-escape
                   (push #\; acc)
                   (progn (push (coerce (reverse acc) 'string) items)
                          (setf acc nil))))
          (t (push char acc)))
     finally (progn
               (when acc
                 (push (coerce (reverse acc) 'string) items))
               (return (reverse items)))))

(defun get-strings-key (key file &key (group "Desktop Entry"))
  (let ((str (get-ordered-hash `(,group ,key) (slot-value file 'hash))))
    (when str
      (split-multi-string str))))

(defun get-locale-string-key (key file &key
                                         (group "Desktop Entry")
                                         (locales (lc-messages-to-locales)))
  (loop for locale in locales
     for value = (get-ordered-hash (append `(,group ,key) `(,locale))
                                   (slot-value file 'hash))
     when value
     do (return (replace-escapes value))
     finally (return (get-string-key key file :group group))))

(defun get-locale-strings-key (key file &key
                                          (group "Desktop Entry")
                                          (locales (lc-messages-to-locales)))
  (loop for locale in locales
     for value = (get-ordered-hash (append `(,group ,key) `(,locale))
                                   (slot-value file 'hash))
     when value
     do (return (split-multi-string value))
     finally (return (get-strings-key key file :group group))))

(defun get-boolean-key (key file &key (group "Desktop Entry"))
  (let ((value (get-ordered-hash `(,group ,key) (slot-value file 'hash))))
    (when value
      (cond
        ((string= value "false") nil)
        ((string= value "true") t)
        (t (error "malformed boolean"))))))

(defun get-number-key (key file &key (group "Desktop Entry"))
  "PARSE-NUMBER:PARSE-NUMBER doesn't _quite_ implement the semantics
of strtod/sscanf, but it's portable.  The desktop file spec doesn't
define any standard number keys anyway."
  (let ((value (get-ordered-hash `(,group ,key) (slot-value file 'hash))))
    (when value
      (parse-number:parse-real-number value))))

(defun load-desktop-files (&optional (subdir #P"applications/"))
  "Load desktop files from SUBDIR underneath $XDG_DATA_HOME and each
  of $XDG_DATA_DIRS.  Desktop files found under #P\"applications/\"
  have IDs; files earlier in the search path take precedence over
  files later in the search path with the same ID."
  ;; FIXME: this is hideous code
  (loop for (file . id) in (mapcan (lambda (dir)
                                     (let ((subdir (uiop:merge-pathnames* subdir dir)))
                                       (mapcar (lambda (file)
                                                 (cons file (replace-all (namestring (uiop:subpathp file subdir)) "/" "-")))
                                               (uiop:directory*
                                                (uiop:merge-pathnames*
                                                 "*.desktop"
                                                 (uiop:wilden subdir))))))
                                   (cons (uiop:xdg-data-home) (uiop:xdg-data-dirs)))
     with hash = (make-hash-table :test 'equal)
     unless (gethash id hash)
     do (let ((desktop-file (load-desktop-file file)))
          (setf (gethash id hash) desktop-file
                (slot-value desktop-file 'id) id))
     finally (return (make-instance 'desktop-files :files hash))))

(defun find-desktop-file-by-id (files id)
  "Find the desktop file with the given ID in FILES."
  (gethash id (slot-value files 'files)))

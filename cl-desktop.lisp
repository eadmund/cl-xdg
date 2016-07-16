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

;; test-helper.el --- Test helpers for test-runner  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damien Merenne

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defconst test-runner-test-path
  (file-name-as-directory (file-name-directory (or load-file-name
                                                   buffer-file-name)))
  "The test directory.")
(defconst test-runner-test-data-path (file-name-as-directory (concat test-runner-test-path "data"))
  "The test data directory.")
(defconst test-runner-root-path
  (file-name-as-directory (file-name-directory (directory-file-name test-runner-test-path)))
  "The test-runner project root path.")
(add-to-list 'load-path test-runner-root-path)

(defmacro test-runner-with-test-content (file-name &rest body)
  "Setup a buffer backing FILE-NAME with CONTENT and run BODY in it."
  (declare (indent 1))
  `(let ((file-path (concat test-runner-test-data-path ,file-name)))
     (unless (file-exists-p file-path)
       (error
        "File %s does not exists"
        file-path))
     (save-excursion (with-current-buffer (find-file-noselect file-path)
                       (goto-char (point-min)) ,@body (kill-buffer)))))

(provide 'test-helper)
;;; test-helper.el ends here

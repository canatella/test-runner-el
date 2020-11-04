;;; test-runner-ert.el -- Test runner for ert -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Damien Merenne <dam@cosinux.org>

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

;;

;;; Code:

(require 'test-runner)
(require 'cl-lib)
(require 'project)
(require 'seq)

(defgroup test-runner-ert nil "Running ert tests from emacs." :group 'test-runner)

(defcustom test-runner-ert-load-path
  '(".")
  "Additional path to search when running tests (see `load-path')."
  :group 'test-runner-ert
  :type '(list directory))

(defcustom test-runner-ert-test-sexps '(ert-deftest)
  "The list of symbol that defines a ERT test."
  :group 'test-runner-ert
  :type '(list symbol))

(defcustom test-runner-ert-test-directory "test/"
  "The relative project path to the directory holding tests."
  :group 'test-runner-ert
  :type 'string)

(defcustom test-runner-ert-test-file-regex "-test\\.el$"
  "Regular expression to lookup ERT test files in the `test-runner-ert-test-directory'."
  :group 'test-runner-ert
  :type 'regexp)

(defun test-runner-ert-enable-p ()
  "Return non nil if there are ert test in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward (regexp-opt (seq-map #'symbol-name test-runner-ert-test-sexps)) nil t)))

(defclass test-runner-backend-ert (test-runner-backend-exec) ((enable :initform #'test-runner-ert-enable-p))
  "Test runner backend that runs Emacs ERT tests.

Besides the customization variables, the following method or
properties can be customized:

`test-runner-backend-ert-test-helper' or `:ert-test-helper'

  Return a test helper elisp file to load before tests. The
  default method returns \"test/test-helper.el\" if it
  exists. See also `test-runner-ert-test-directory'.

`test-runner-backend-ert-load-path' or `:ert-load-path'

  The load path to setup in the test Emacs instance for loading
lisp dependencies. The default method returns
`test-runner-ert-load-path'.

`test-runner-backend-ert-test-files' or `:ert-test-files'

  Returns the list of test files for the project. The default
  method list files in directory `test-runner-ert-test-directory'
  that match `test-runner-ert-test-file-regex'.

`test-runner-backend-exec-binary' or `:exec-binary'

  Return the Emacs binary to use to run tests. The default method
  returns the current Emacs instance executable path.")

(cl-defmethod test-runner-backend-ert-sexp-at-point ((_backend test-runner-backend-ert))
  "Return current test at point."
  (save-excursion
    (beginning-of-defun)
    (when (looking-at-p "(")
      (let ((start (point)))
        (forward-sexp)
        (car (read-from-string (buffer-substring start (point))))))))

(cl-defmethod test-runner-backend-ert-test-helper ((_backend test-runner-backend-ert))
  "Look for a test-helper file in project test directory."
  (ignore-error 'file-missing
    (if-let ((helper
              (car
               (directory-files
                (concat (project-root (project-current)) test-runner-ert-test-directory)
                nil
                "test-helper\\.el"))))
        (concat test-runner-ert-test-directory helper))))

(cl-defmethod test-runner-backend-ert-load-path ((_backend test-runner-backend-ert))
  "Return Emacs `load-path' for running ert tests."
  test-runner-ert-load-path)

(cl-defmethod test-runner-backend-ert-test-helper-args ((backend test-runner-backend-ert))
  "Return Emacs BACKEND argument to load test helper."
  (if-let ((test-helper (test-runner-backend-ert-test-helper backend)))
      `("-l" ,test-helper)))

(cl-defmethod test-runner-backend-ert-load-path-args ((backend test-runner-backend-ert))
  "Return Emacs BACKEND command line arguments to configure `load-path'."
  (seq-mapcat (apply-partially #'list "-L") (test-runner-backend-ert-load-path backend)))

(cl-defmethod test-runner-backend-ert-test-files ((_backend test-runner-backend-ert))
  "Return a list of test files."
  (ignore-error 'file-missing
    (let ((files
           (directory-files
            (concat (project-root (project-current)) test-runner-ert-test-directory)
            nil
            test-runner-ert-test-file-regex)))
      (seq-map (apply-partially #'concat test-runner-ert-test-directory) files))))

(cl-defmethod test-runner-backend-ert-launch-form ((backend test-runner-backend-ert)
                                                   selector)
  "Return a form to run BACKEND tests with SELECTOR."
  `(let ((ert-quiet ,(oref backend verbose)))
     (ert-run-tests-batch-and-exit ,selector)))

(cl-defmethod test-runner-backend-exec-binary ((_backend test-runner-backend-ert))
  "Return the Emacs binary to use for running tests."
  (expand-file-name invocation-name invocation-directory))

(cl-defmethod test-runner-backend-exec-command ((backend test-runner-backend-ert) selector &rest arguments) ;nofmt
  "Return the BACKEND command to run test in this project, with SELECTOR and additional ARGUMENTS."
  (let ((test-helper (test-runner-backend-ert-test-helper-args backend))
        (load-path (test-runner-backend-ert-load-path-args backend))
        (launch-form (prin1-to-string (test-runner-backend-ert-launch-form backend selector))))
    (apply #'cl-call-next-method backend
           `("-batch" ,@load-path "-l" "ert" ,@test-helper ,@arguments "--eval" ,launch-form))))

(cl-defmethod test-runner-backend-exec-arguments-test-at-point ((backend test-runner-backend-ert)) ;nofmt
  "Return BACKEND test definition for test at point."
  (let ((sexp (test-runner-backend-ert-sexp-at-point backend)))
    (when (and sexp (seq-contains-p test-runner-ert-test-sexps (car sexp)))
      (let ((pattern
             (format "^%s$" (regexp-quote (format "%s" (cadr sexp)))))
            (file (buffer-file-name)))
        (list pattern "-l" file)))))

(cl-defmethod test-runner-backend-exec-arguments-test-file ((backend test-runner-backend-ert)) ;nofmt
  "Return BACKEND test definition for current file."
  (when (and (string-match-p test-runner-ert-test-file-regex (buffer-file-name))
             (locate-dominating-file (buffer-file-name) test-runner-ert-test-directory))
    (list nil "-l" (buffer-file-name))))

(cl-defmethod test-runner-backend-exec-arguments-test-project ((backend test-runner-backend-ert)) ;nofmt
  "Return BACKEND test definition for current file."
  (cons nil (seq-mapcat (apply-partially #'list "-l") (test-runner-backend-ert-test-files backend))))

(test-runner-define-compilation-mode ert)
(add-to-list 'test-runner-backends 'ert)

(provide 'test-runner-ert)

;;; test-runner-ert.el ends here

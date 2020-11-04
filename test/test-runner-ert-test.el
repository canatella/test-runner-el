;;; cerbere-elisp-ert-runner-test.el -- Ert runner Cerbere backend tests.

;; Copyright (C) 2018 Damien Merenne <dam@cosinux.org>

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

;; Tests for the elisp ert-runner Cerbere backend.

;;; Code:

(require 'test-helper)
(require 'test-runner-ert)

(defvar ert-backend (test-runner-backend-ert) "Backend to test against.")

(defun strip (string)
  "Remove the Emacs binary path form STRING."
  (replace-regexp-in-string test-runner-root-path ""
                            (replace-regexp-in-string (regexp-quote (expand-file-name invocation-name invocation-directory)) "" string)))

(ert-deftest test-test-runner-backend-ert-sexp-at-point ()
  (test-runner-with-test-content "test/example-test.el"
    (forward-line 9)
    (should (equal '(ert-deftest test-example ()
                      (should t))
                   (test-runner-backend-ert-sexp-at-point ert-backend)))))

(ert-deftest test-test-runner-backend-ert-test-at-point ()
  (test-runner-with-test-content "test/example-test.el"
    (forward-line 9)
    (should (equal " -batch -L . -l ert -l test/test-helper.el -l test/data/test/example-test.el --eval \\(let\\ \\(\\(ert-quiet\\ nil\\)\\)\\ \\(ert-run-tests-batch-and-exit\\ \\\"\\^test-example\\$\\\"\\)\\)"
                   (strip (test-runner-backend-test-at-point ert-backend))))
    (forward-line 3)
    (should-not (test-runner-backend-test-at-point ert-backend))
    (goto-char (point-min))
    (should-not (test-runner-backend-test-at-point ert-backend))))

(ert-deftest test-test-runner-backend-ert-test-for-file ()
  (test-runner-with-test-content "test/example-test.el"
    (should (equal " -batch -L . -l ert -l test/test-helper.el -l test/data/test/example-test.el --eval \\(let\\ \\(\\(ert-quiet\\ nil\\)\\)\\ \\(ert-run-tests-batch-and-exit\\ nil\\)\\)"
                   (strip (test-runner-backend-test-file ert-backend)))))
  (test-runner-with-test-content "example.el"
    (should-not (test-runner-backend-test-file ert-backend))))

(ert-deftest test-test-runner-backend-ert-test-for-project ()
  (test-runner-with-test-content "test/example-test.el"
    (should (equal " -batch -L . -l ert -l test/test-helper.el -l test/test-runner-ert-test.el --eval \\(let\\ \\(\\(ert-quiet\\ nil\\)\\)\\ \\(ert-run-tests-batch-and-exit\\ nil\\)\\)"
                   (strip (test-runner-backend-test-project ert-backend))))))

;;; test-runner-ert-test.el ends here

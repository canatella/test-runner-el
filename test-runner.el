;;; test-runner.el --- Builds unit test runners -*- lexical-binding: t; -*-

;; Author: Damien Merenne
;; URL: https://github.com/canatella/test-runner
;; Created: 2020-03-11
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))

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

;; This package provides a macro to easily define a mode to integrate Emacs with a unit test
;; framework. Depending on which features are provided, the generated framwork allows running a
;; project tests, a project file or a project unit test.

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'compile)
(require 'subr-x)
(require 'project)

(defgroup test-runner nil "Running tests from Emacs." :group 'convenience)

(defcustom test-runner-key-prefix nil
  "Prefix of test runner commands."
  :type 'key-sequence
  :group 'test-runner)

(defcustom test-runner-backend 'none "Default test runner backend to use." :type 'symbol)
(put 'test-runner-backend 'safe-local-variable #'symbolp)

(defcustom test-runner-backends '() "List of active test runner backends." :type '(list symbol))

(defvar-local
  test-runner-current-backend
  '()
  "The backend to use for running tests for this file.")

(defvar test-runner-last-test-command
  '()
  "The last test command that was executed so that we can execute it again.")

(defun test-runner-backend-enable-p (backend)
  "Return non nil if BACKEND should be enabled for current buffer."
  (let ((pred (oref backend enable)))
    (cond
     ((stringp pred)
      (and (buffer-file-name) (string-match-p pred (buffer-file-name))))
     ((functionp pred)
      (funcall pred))
     ((not pred))
     (t (error "Invalid enable predicate %s for backend %s" pred (oref backend name))))))

(defun test-runner-make-instance (sym)
  "Return a new backend instance for SYM."
  (funcall
   (intern (format "test-runner-backend-%s" sym))
   :name (symbol-name sym)
   :buffer (current-buffer)))

(defun test-runner-make-backend ()
  "Return a new backend instance for current buffer."
  (or
   (seq-find #'test-runner-backend-enable-p
             (seq-map #'test-runner-make-instance test-runner-backends))
   (test-runner-make-instance test-runner-backend)))

(defun test-runner-current-backend ()
  "Return the current backend instance or error out if not set."
  (or test-runner-current-backend
      (user-error "Test runner backend is not configured for current buffer")))

(defclass test-runner-backend-none ()
  ((name :type string :initarg :name)
   (buffer :type buffer :initarg :buffer :documentation "The buffer associated to this test.")
   (enable :initarg :enable :initform nil :documentation "If a regular expression, use this backend if the regular expression matches against `buffer-file-name' . If a function, enable this backend if it returns non-nil.")
   (verbose :type
            (or null symbol)
            :initform ()
            :documentation "When non-nil, running test will produce a more verbose output."))
  "Do nothing test runner backend.

The `none' backend is the most basic test runner backend: it
doesn't run anything but it gives you complete freedom. To
define a new backend on top of this one, either use
`cl-defmethod' or provide method bodies in `test-runner-define' for
the following methods:

`test-runner-backend-test-at-point' or `:test-at-point'

  Should return a command that when executed will run the test at
  point.

`test-runner-test-file' or `:test-file'

  Should return a command that when executed will run the tests
  for the current buffer file.

`test-runner-test-project' or `:test-project'

  Should return a command that when executed will r|n the tests
  for the current project.

The following methods can be customized:

`test-runner-backend-compilation-mode' or `:compilation-mode'

  Returns the compilation mode (see `define-compilation-mode')
  associated to this backend. When using `test-runner-define' or
  `test-runner-define-compilation-mode', a compilation mode
  dedicated to the backend will be defined using
  `define-compilation-mode' and will be returned by this
  function. This allows you to customize the error matching and
  all the specific compilation related variable for the
  backend.

`test-runner-backend-compilation-buffer-name' or `:compilation-buffer-name'

  Returns the compilation buffer name to use for this backend.")

(cl-defmethod
  test-runner-backend-compilation-mode
  ((backend test-runner-backend-none))
  "Return the compilation mode for this BACKEND."
  (intern (format "test-runner-%s-compilation-mode" (oref backend name))))

(cl-defmethod
  test-runner-backend-compilation-buffer-name
  ((backend test-runner-backend-none)
   _mode)
  "Return the compilation buffer name for this BACKEND."
  (format "*test runner: %s*" (oref backend name)))

(cl-defmethod
  test-runner-backend-test-at-point
  ((_backend test-runner-backend-none))
  "Should return a command to run test at point."
  nil)

(cl-defmethod
  test-runner-backend-test-file
  ((_backend test-runner-backend-none))
  "Should return a command to run tests for current file."
  nil)

(cl-defmethod
  test-runner-backend-test-project
  ((_backend test-runner-backend-none))
  "Should return a command to run tests for current project."
  nil)

(defclass test-runner-backend-exec (test-runner-backend-none) ()
  "Test runner backend using a specific executable.

Extend this backend if the tests are run using an executable and
passing arguments to it. The following method or
`test-runner-define' properties must be provided.

`test-runner-backend-exec-binary' or `:exec-binary'

  Should return the binary to execute for running tests.

`test-runner-backend-exec-arguments-test-at-point' or
`:exec-arguments-test-at-point'

  Should return a list of string where each item is an argument
  to be passed to the test executable for running the test at point.

`test-runner-backend-exec-arguments-test-file' or
`:exec-arguments-test-file'

  Should return a list of string where each item is an argument
  to be passed to the test executable for running the tests in
  the current buffer file.

`test-runner-backend-exec-arguments-test-project' or
`:exec-arguments-test-project'

  Should return a list of string where each item is an argument
  to be passed to the test executable for running the tests in
  the current project.")

(cl-defmethod test-runner-backend-exec-binary ((_backend test-runner-backend-exec))
  "Should return the executable to run for tests.")

(cl-defmethod test-runner-backend-exec-arguments-test-at-point ((_backend test-runner-backend-exec))
  "Should return the arguments to pass to binary to run test at point.")

(cl-defmethod test-runner-backend-exec-environment-test-at-point ((_backend test-runner-backend-exec))
  "Should return the environment to pass to binary to run test at point.")

(cl-defmethod test-runner-backend-exec-arguments-test-file ((_backend test-runner-backend-exec))
  "Should return the arguments to pass to binary to run tests for current file.")

(cl-defmethod test-runner-backend-exec-environment-test-file ((_backend test-runner-backend-exec))
  "Should return the environment to pass to binary to run tests for current file.")

(cl-defmethod test-runner-backend-exec-arguments-test-project ((_backend test-runner-backend-exec))
  "Should return the arguments to pass to binary to run tests for current project.")

(cl-defmethod test-runner-backend-exec-environment-test-project ((_backend test-runner-backend-exec))
  "Should return the environment to pass to binary to run tests for current project.")

(cl-defmethod test-runner-backend-exec-command ((backend test-runner-backend-exec) arguments environment)
  "Should return the command to run BACKEND tests with ARGUMENTS and ENVIRONMENT."
  (let ((binary (test-runner-backend-exec-binary backend))
        (arguments (seq-map #'shell-quote-argument (seq-filter #'identity arguments)))
        (environment (seq-map #'shell-quote-argument (seq-filter #'identity environment))))
    (format "env %s %s %s" (string-join environment " ") binary (string-join arguments " "))))

(cl-defmethod test-runner-backend-test-at-point ((backend test-runner-backend-exec))
  "Return the command to run BACKEND test at point."
  (if-let ((arguments (test-runner-backend-exec-arguments-test-at-point backend))
           (environment (test-runner-backend-exec-environment-test-at-point backend)))
      (funcall #'test-runner-backend-exec-command backend arguments environment)))

(cl-defmethod test-runner-backend-test-file ((backend test-runner-backend-exec))
  "Return the command to run BACKEND tests for current file."
  (if-let ((arguments (test-runner-backend-exec-arguments-test-file backend))
           (environment (test-runner-backend-exec-environment-test-file backend)))
      (funcall #'test-runner-backend-exec-command backend
               arguments environment)))

(cl-defmethod test-runner-backend-test-project ((backend test-runner-backend-exec))
  "Return the command to run BACKEND tests for current project."
  (if-let ((arguments (test-runner-backend-exec-arguments-test-project backend))
           (environment (test-runner-backend-exec-environment-test-project backend)))
      (funcall #'test-runner-backend-exec-command backend
             arguments environment)))

(defclass test-runner-backend-compile (test-runner-backend-exec) ()
  "Test runner backend based on the `exec' backend but which also
runs a compilation command before running tests. By default it
will run `compilation-command' but this can be customized by
providing the `test-runner-backend-compile-command' method or
`:compile-command' property.

See `test-runner-backend-exec'.")

(cl-defmethod test-runner-backend-compile-command ((_backend test-runner-backend-compile))
  "Return the command to build current test."
  (or compile-command (user-error "Cannot compile tests, `compile-command' is not set")))

(cl-defmethod test-runner-backend-exec-command ((backend test-runner-backend-compile)
                                                &rest arguments)
  "Return a command combining `compile-command' and running BACKEND tests with ARGUMENTS."
  (let ((compile-command (test-runner-backend-compile-command backend))
        (test-command (apply #'cl-call-next-method backend arguments)))
    (format "%s && %s" compile-command test-command)))

(defun test-runner-propertize-command (command)
  "At the current backend as a property to COMMAND."
  (when command (propertize command 'test-runner-backend (test-runner-current-backend))))

(defun test-runner-test-at-point ()
  "Return command to run test at point."
  (test-runner-propertize-command
   (test-runner-backend-test-at-point (test-runner-current-backend))))

(defun test-runner-test-file ()
  "Return command to run tests for current file."
  (test-runner-propertize-command (test-runner-backend-test-file (test-runner-current-backend))))

(defun test-runner-test-project ()
  "Return command to run tests for current project."
  (test-runner-propertize-command (test-runner-backend-test-project (test-runner-current-backend))))

(defun test-runner-setup ()
  "Setup test runner for current buffer."
  (setq-local test-runner-current-backend (test-runner-make-backend))
  (message "Using test runner backend %s" (oref test-runner-current-backend name)))

(defun test-runner-teardown ()
  "Disable test runner for current buffer."
  (setq-local test-runner-current-backend nil))

(defun test-runner-run (command)
  "Run test COMMAND."
  (let* ((default-directory (project-root (project-current)))
         (backend (get-text-property 0 'test-runner-backend command))
         (compilation-buffer-name-function
          (apply-partially #'test-runner-backend-compilation-buffer-name backend))
         (compilation-buffer-mode (test-runner-backend-compilation-mode backend)))
    (setq test-runner-last-test-command command)
    (save-some-buffers (not compilation-ask-about-save) compilation-save-buffers-predicate)
    (compilation-start command compilation-buffer-mode)))

(defun test-runner-run-test-at-point ()
  "Run test at point."
  (interactive)
  (if-let ((command (test-runner-test-at-point)))
      (test-runner-run command)
    (user-error "No test to run at current point")))

(defun test-runner-run-test-file ()
  "Run tests in current file."
  (interactive)
  (if-let ((command (test-runner-test-file)))
      (test-runner-run command)
    (user-error "No test to run in current file")))

(defun test-runner-run-test-project ()
  "Run tests in current project."
  (interactive)
  (if-let ((command (test-runner-test-project)))
      (test-runner-run command)
    (user-error "No test to run in current project")))

(defun test-runner-run-last ()
  "Rerun last command."
  (interactive)
  (unless test-runner-last-test-command (user-error "No known last test runner command to rerun"))
  (test-runner-run test-runner-last-test-command))

(defun test-runner-run-dwim ()
  "Try to run test at point if it exists, otherwise run the last command."
  (interactive)
  (if-let ((command (test-runner-test-at-point)))
      (test-runner-run command)
    (test-runner-run-last)))

(defvar test-runner-local-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ".") #'test-runner-run-test-at-point)
    (define-key map (kbd "f") #'test-runner-run-test-file)
    (define-key map (kbd "l") #'test-runner-run-last)
    (define-key map (kbd "p") #'test-runner-run-test-project)
    (define-key map (kbd "t") #'test-runner-run-dwim)
    map)
  "Keymap for running tests.")

(define-minor-mode test-runner-mode
  "Run test in current buffer using `test-runner-backend'.

You can customize which backend to use by setting
`test-runner-backend'.  For setting it up for a project or a
directory, use `add-dir-local-variable' to configure it."
  :init-value nil
  (when test-runner-key-prefix (local-set-key test-runner-key-prefix test-runner-local-map))
  (if test-runner-mode (test-runner-setup) (test-runner-teardown)))

(defmacro test-runner-define-compilation-mode (name &rest body)
  "Defines a new compilation mode for test backend with NAME."
  (declare (indent 1))
  (let ((compilation-mode-name (intern (format "test-runner-%s-compilation-mode" name)))
        (compilation-buffer-name (symbol-name name)))
    `(define-compilation-mode ,compilation-mode-name ,compilation-buffer-name
       ,(format "Major mode for %s test logs." name)
       ,@body
       (setq-local test-runner-backend (quote ,name))
       (test-runner-mode))))

(cl-defmacro test-runner-define (name parent doc-string &rest methods &key enable &allow-other-keys)
  "Defines a test runner backend with NAME based on PARENT.

DOC-STRING should be the documentation for the backend.

ENABLE is either a regular expression or a function.  This
backend will be enabled for buffer where the file name matches
the regular expression or the function returns non-nil.

METHODS is a property list of method to override in parent
backend.  The value of each property will be evaluated when
trying to run a test.

As an example, to implement a backend that would be running the
hypothetical `foo' binary with some arguments given the context:

  (test-runner-define foo exec
    \"Runs tests with foo.\"
    :binary \"foo\"
    :test-at-point `(\"--name\" ,(my-test-name))
    :test-file `(\"--file\" ,(my-test-file))
    ...)

Lookup each backend documentation to see which property they
support."
  (declare (indent 2) (doc-string 3))
  (let* ((class-name (intern (format "test-runner-backend-%s" name)))
         (parent-name (intern (format "test-runner-backend-%s" parent)))
         (method-forms
          (seq-map
           (lambda (method)
             (let ((method-name
                    (intern
                     (format "test-runner-backend-%s"
                             (intern (substring (symbol-name (car method)) 1))))))
               `(cl-defmethod ,method-name ((_backend ,class-name)) ,@(cdr method))))
           (seq-partition methods 2))))
    `(progn
       (test-runner-define-compilation-mode ,name)
       (defclass ,class-name (,parent-name)
         ((enable :initform ,enable))
         ,doc-string)
       ,@method-forms)))
(provide 'test-runner)

;;; test-runner.el ends here

(require 'ert)
(require 'pb-scripts)
(require 'pb-run-tests)

(require 'test-pb-init)
(require 'pb-fixtures)
(require 'with-fixtures)

(defun mock-setup-file () "data/setup.py")
(defun mock-python-buffer-name () "python_example.py")
(defun mock-python-example-file () (concat (file-name-as-directory "data")
                                           (mock-python-buffer-name)))

(defun mock-pb-test-success-message () "[emacs.test.build-result],fcrepo-solr-testrunner, 0.1, 7, 0, OK, 0.019, OK")


(defun fixture-python-code-buffer (body)
  "Fixture providing python code buffer called python_example.py"
  (save-window-excursion
    (find-file (mock-python-example-file))
    (switch-to-buffer (mock-python-buffer-name))
    (goto-char (point-min))
    (unwind-protect
        (funcall body)
      (progn (sleep-for 0 300)
             (kill-buffer (mock-python-buffer-name))))))

(ert-deftest test-pb-name-of-def-or-class-returns-expected-def ()
  "Test that pb-name-of-def-or-class returns the expected def"
  (pb-fix-temp-buffer
   (lambda ()

     (re-search-forward "\"function")
     (should (string= "my_function" (pb-name-of-def-or-class "def"))))
   (pb-fix-org-src-file)))

(ert-deftest test-pb-name-of-def-or-class-returns-expected-method ()
  "Test that pb-name-of-def-or-class returns the expected method"
  (pb-fix-temp-buffer
   (lambda ()
     (re-search-forward "\"method")
     (should (string= "my_method" (pb-name-of-def-or-class "def"))))
   (pb-fix-org-src-file)))

(ert-deftest test-pb-name-of-def-or-class-returns-expected-class ()
  "Test that pb-name-of-def-or-class returns the expected class"
  (pb-fix-temp-buffer
   (lambda ()
     (re-search-forward "\"method")
     (pb-name-of-def-or-class "class")
     (should (string= "MyClass" (pb-name-of-def-or-class "class"))))
   (pb-fix-org-src-file)))

(ert-deftest test-pb-name-of-def-or-class-raises-error-if-no-type-is-found ()
  "Test that pb-name-of-def-or-class raises if no type is found"
  (pb-fix-temp-buffer
   (lambda ()
     (should-error (pb-name-of-def-or-class "class")))
   (pb-fix-org-src-file)))

(ert-deftest test-pb-module-path-returns-the-expected-path ()
  "Test that pb-module-path returns the expected path"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))
    (pb-fix-temp-buffer
     (lambda ()
       (should-error (pb-name-of-def-or-class "class")))
     (cdr (assoc 'src-file files)))))

(ert-deftest test-pb-paths-to-test-under-point-produces-expected-paths ()
  "Test that pb-paths-to-test-under-point returns the expected paths"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))
    (pb-fix-temp-buffer
     (lambda ()
       (re-search-forward "\"method")
       (let ((result (pb-paths-to-test-under-point)))

         (should (string= "example_project" (nth 1 result)))
         (should (string= "example_project.example" (nth 2 result)))
         (should (string= "example_project.example:MyClass" (nth 3 result)))
         (should (string= "example_project.example:MyClass.my_method" (nth 4 result))))

       ) (cdr (assoc 'src-file files)))))

(ert-deftest test-pb-run-test-under-point-calls-shell-command-to-string-with-expected-args-when-path-type-is-provides ()
  "Test that pb-run-test-under-point calls shell-command-to-string where path-type is 'provides'"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))
    (pb-fix-temp-buffer
     (lambda ()

       (re-search-forward "\"method")
       (let ((shell-command-args))
         (flet ((shell-command-to-string (arg) (setq shell-command-args arg) (mock-pb-test-success-message)))

           (pb-run-test-under-point "provides")

           (should (string= (format "python %s -p emacs --test %s %s" (pb-pybuilder-script) "example_project" (cdr (assoc 'project-dir files)))
                            shell-command-args))))

       ) (cdr (assoc 'src-file files)))))

(ert-deftest test-pb-run-test-under-point-calls-shell-command-to-string-with-expected-args-when-path-type-is-package ()
  "Test that pb-run-test-under-point calls shell-command-to-string where path-type is package"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))
    (pb-fix-temp-buffer
     (lambda ()

       (re-search-forward "\"method")
       (let ((shell-command-args))
         (flet ((shell-command-to-string (arg) (setq shell-command-args arg) (mock-pb-test-success-message)))

           (pb-run-test-under-point "package")

           (should (string= (format "python %s -p emacs --test %s %s" (pb-pybuilder-script) "example_project.example" (cdr (assoc 'project-dir files)))
                            shell-command-args))))

       ) (cdr (assoc 'src-file files)))))

(ert-deftest test-pb-run-test-under-point-calls-shell-command-to-string-with-expected-args-when-path-type-is-class ()
  "Test that pb-run-test-under-point calls shell-command-to-string where path-type is class"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))
    (pb-fix-temp-buffer
     (lambda ()

       (re-search-forward "\"method")
       (let ((shell-command-args))
         (flet ((shell-command-to-string (arg) (setq shell-command-args arg) (mock-pb-test-success-message)))

           (pb-run-test-under-point "class")

           (should (string= (format "python %s -p emacs --test %s %s" (pb-pybuilder-script) "example_project.example:MyClass" (cdr (assoc 'project-dir files)))
                            shell-command-args))))

       ) (cdr (assoc 'src-file files)))))

(ert-deftest test-pb-run-test-under-point-calls-shell-command-to-string-with-expected-args-when-path-type-is-def ()
  "Test that pb-run-test-under-point calls shell-command-to-string where path-type is def"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))
    (pb-fix-temp-buffer
     (lambda ()

       (re-search-forward "\"method")
       (let ((shell-command-args))
         (flet ((shell-command-to-string (arg) (setq shell-command-args arg) (mock-pb-test-success-message)))

           (pb-run-test-under-point "def")

           (should (string= (format "python %s -p emacs --test %s %s"
                                    (pb-pybuilder-script)
                                    "example_project.example:MyClass.my_method"
                                    (cdr (assoc 'project-dir files)))
                            shell-command-args))))

       ) (cdr (assoc 'src-file files)))))

(ert-deftest test-pb-run-test-under-point-calls-compile-when-flag-is-set2 ()
  "Test that pb-run-test-under-point calls compile when option is set"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))
    (pb-fix-temp-buffer
     (lambda ()

       (re-search-forward "\"method")
       (let ((shell-command-args) (compile-command-args))
         (flet ((shell-command-to-string (arg) (setq shell-command-args arg) (mock-pb-test-success-message))
                (compile (arg) (setq compile-command-args arg) nil))
           
           (pb-run-test-under-point "def" t)

           (should (string= (format "%s %s.compile" (pb-compiler-script) (file-name-as-directory  (cdr (assoc 'project-dir files))))
                                compile-command-args))))
       
       ) (cdr (assoc 'src-file files)))))


(provide 'test-pb-run-tests)

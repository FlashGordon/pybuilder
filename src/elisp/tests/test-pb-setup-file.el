(require  'ert)
(require 'pb-setup-file)
(require 'el-mock)
(require 'with-fixtures)
(require 'test-pb-init)


(defun fixture-temporary-dir (body)
  "Fixture providing temporary dir"
  (let ((temp-dir (file-name-as-directory (make-temp-file "pb-test" t))))
    (unwind-protect
        (funcall body temp-dir)
      (delete-directory temp-dir t))))


(defun pb-mock-setup-file () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/example-project/setup.py"))
(defun pb-mock-setup-folder () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/example-project/"))
(defun pb-mock-broken-setup-file () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/broken_setup.py"))

(defun pb-touch (path)
  (with-temp-buffer
    (append-to-file (point-min) (point-max) path)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ert-deftest find-setup-file-in-same-dir ()
  "Test that pb-setup-file-find-setup-file finds setup file if
   present in same dir as buffer"
  (with-fixtures* ((tmp-dir (fixture-temporary-dir)))
    (let ((setup-file (concat (expand-file-name tmp-dir) "setup.py"))
          (test-file(concat (expand-file-name tmp-dir) "test_file.py")))

      ;; given
      (copy-file (pb-mock-setup-file) setup-file)
      (pb-touch test-file)

      (find-file test-file)
      (let ((buffer (current-buffer)))
        (unwind-protect
            ;;when
            (setq returned-setup-file (pb-setup-file-find-setup-file))
          (progn (sleep-for 0 300)
                 (kill-buffer buffer))))
      ;;then
      (should (string= setup-file returned-setup-file)))))


(ert-deftest find-setup-file-in-parent-dir-path ()
  "Test that pb-setup-file-find-setup-file finds setup file if
   present in parent dir path"
  (with-fixtures* ((tmp-dir (fixture-temporary-dir)))
    (let ((setup-file (concat (expand-file-name tmp-dir) "setup.py"))
          (test-file(concat (expand-file-name tmp-dir) "src/test_file.py")))
      (make-directory (concat (expand-file-name tmp-dir) "src"))

      ;; given
      (copy-file (pb-mock-setup-file) setup-file)
      (pb-touch test-file)

      (find-file test-file)
      (let ((buffer (current-buffer)))
        (unwind-protect
            ;;when
            (setq returned-setup-file (pb-setup-file-find-setup-file))
          (progn (sleep-for 0 300)
                 (kill-buffer buffer))))
      ;;then
      (should (string= setup-file returned-setup-file)))))

(ert-deftest find-setup-file-raises-error-if-no-setup-file-is-found ()
  "Test that pb-setup-file-find-setup-file raises an error if no
   setup file is found in parent dir path"
  (with-fixtures* ((tmp-dir (fixture-temporary-dir)))
    (let ((test-file(concat (expand-file-name tmp-dir) "test_file.py")))      
      ;; given
      (pb-touch test-file)

      (find-file test-file)
      (let ((buffer (current-buffer)))
        (unwind-protect
            ;;when/then
            (should-error (pb-setup-file-find-setup-file))

          (progn (sleep-for 0 300)
                 (kill-buffer buffer)))))))

(ert-deftest find-setup-file-returns-expected-folder-when-return-directory-option-is-used ()
  "Test that pb-setup-file-find-setup-file returns the expected
   path when dir return-directory option is used"
  (with-fixtures* ((tmp-dir (fixture-temporary-dir)))
    (let ((setup-file (concat (expand-file-name tmp-dir) "setup.py"))
          (test-file(concat (expand-file-name tmp-dir) "test_file.py")))

      ;; given
      (copy-file (pb-mock-setup-file) setup-file)
      (pb-touch test-file)

      (find-file test-file)
      (let ((buffer (current-buffer)))
        (unwind-protect
            ;;when
            (setq returned-setup-file (pb-setup-file-find-setup-file t))
          (progn (sleep-for 0 300)
                 (kill-buffer buffer))))
      ;;then
      (should (string= tmp-dir returned-setup-file)))))

(ert-deftest create-project-info-returns-expected-project ()
  "Test whether the create-project-info function returns info for the expected project"
  (should (string= "example-project" (cdr (assoc "name" (pb-setup-file-create-project-info (pb-mock-setup-file)))))))

(ert-deftest create-project-info-result-contains-expected-entrys ()
  "Test whether the returned result contains the expected entrys"
  (let ((expected-keys (list "test-suite" "package-dir" "setup-file" "provides" "setup-dir" "version" "src-dir" "name"))
        (result (pb-setup-file-create-project-info (pb-mock-setup-file))))
    (mapcar (lambda (key) (should (member key expected-keys))) (mapcar 'car result))))

(ert-deftest create-project-info-raises-error-if-setup-file-does-not-exist ()
  "Test that create-project-info raises an error if setup file does not exist."
  (should-error (pb-setup-file-create-project-info "non-existing-file")))

(ert-deftest create-project-file-raises-if-evaluation-of-setup-file-fails ()
  "test whether create-project-info-raises-if-evaliation-of-setup-file-fails"
  (should-error (pb-setup-file-create-project-info (pb-mock-broken-setup-file))))

(ert-deftest pyenv-string-returns-expected-pyenv-string-if-pyenv-file-exists ()
  "Test whether pyenv-string returns the expected string if pyenv file is present"
  (with-fixtures* ((tmp-dir (fixture-temporary-dir)))
    (let ((pyenv-file (concat (expand-file-name tmp-dir) "pyenv")))

      (pb-touch pyenv-file)
      (should (string= " && source pyenv > /dev/null" (pb-setup-file-pyenv-string tmp-dir))))))

(ert-deftest pyenv-string-returns-empty-string-if-no-pyenv-file-is-present ()
  "Test that pyenv-string returns empty string if no pyenv file is found"
  (with-fixtures* ((tmp-dir (fixture-temporary-dir)))
    (should (string= "" (pb-setup-file-pyenv-string tmp-dir)))))

(ert-deftest call-setup-calls-the-shell-with-the-expected-arguments ()
  "Test whether the call-setup function calls the shell with the expected arguments"

  (let ((shell-command-args))
    (flet ((shell-command-to-string (arg) (setq shell-command-args arg) " foo "))

      (pb-setup-file-call-setup (pb-mock-setup-file) (list "foo" "bar"))
      (should (string= shell-command-args (format "( cd %s && python setup.py foo bar && cd - > /dev/null )" (pb-mock-setup-folder)))))))

(ert-deftest call-setup-calls-returns-chomped-output-from-shell-call ()
  "Test that the return value from call-setup is the expected chomped string"

  (let ((shell-command-args))
    (flet ((shell-command-to-string (arg) (setq shell-command-args arg) " foo "))

      (should (string= "foo" (pb-setup-file-call-setup (pb-mock-setup-file) (list "foo" "bar")))))))

(provide 'test-pb-setup-file)
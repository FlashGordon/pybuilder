(require  'ert)
(require 'el-mock)
(require 'test-pb-init)
(require 'pb-fixtures)
(require ' pb-scripts)
(require 'pb-server)


(defun mock-pb-build-success-message () "[emacs.build.build-result],fcrepo-solr-testrunner, 0.1, 7, 0, OK, 0.246, 85.78%")


(ert-deftest test-pb-set-modeline-status-sets-modeline-message-as-expected ()
  "Tests whether pb-set-modeline-status sets the modeline as expected"
  (let ((minor-mode-alist (cons (list 'pybuilder-mode "MESG" ) (list))))
    (pb-set-modeline-status "NEW-MESG")
    (should (string= "NEW-MESG" (cadr (assoc 'pybuilder-mode minor-mode-alist))))))


(ert-deftest test-pb-server-compile-calls-compile-with-expected-compile-file ()
  "Test that pb-server-compile calls compile with the expected .compile file "
  (pb-fix-temp-dir
   (lambda (temp-dir)

     (let* ((compile-file-path (concat (file-name-as-directory temp-dir) ".compile"))
            (expected-compile-result (concat (pb-compiler-script) " " compile-file-path))
            (actual-compile-result))

       (copy-file (pb-fix-example-compile-file) compile-file-path)
       (flet ((compile (arg) (setq actual-compile-result arg) nil))

         (pb-server-compile temp-dir)
         (should (string= expected-compile-result actual-compile-result)))))))


(ert-deftest test-pb-server-compile-raises-if-no-compile-file-is-present ()
  "Test that pb-server-compile raises an error if no .compile file is present"
  (pb-fix-temp-dir
   (lambda (temp-dir)
     (should-error (pb-server-compile temp-dir)))))

(ert-deftest test-pb-compile-output-filter-calls-compile-with-the-expected-argument ()
  "Test that pb-compile-output-filter calls compile with the expected arguments"
  (let ((pb-current-server-project (list (cons "setup-dir" "my-dir" ) )))
    (with-mock
      (mock (pb-server-compile "my-dir"))
      (pb-compile-output-filter "ORG BUILD OUTPUT"))))

(ert-deftest test-pb-compile-output-filter-removes-hook ()
  "Test that pb-compile-output-filter removes itself from comint-output-filter-functions hooks"
  (let ((pb-current-server-project (list (cons "setup-dir" "my-dir"))))
    (unwind-protect
        (with-mock
          (add-hook 'comint-output-filter-functions 'pb-compile-output-filter)
          (mock (pb-server-compile "my-dir"))

          (should (member 'pb-compile-output-filter comint-output-filter-functions))
          (pb-compile-output-filter "ORG BUILD OUTPUT")
          (should-not (member 'pb-compile-output-filter comint-output-filter-functions)))
      (remove-hook 'comint-output-filter-functions 'pb-compile-output-filter))))

(ert-deftest test-pb-compile-output-filter-pipes-input-trhough-to-output-unmodified ()
  "Test that pb-compile-output-filter pipes the input remains unmodified when returned"
  (let ((pb-current-server-project (list (cons "setup-dir" "my-dir"))))
    (with-mock
      (mock (pb-server-compile "my-dir"))
      (should (string= "ORG BUILD OUTPUT" (pb-compile-output-filter "ORG BUILD OUTPUT"))))))

(ert-deftest test-pb-modeline-output-filter-pipes-input-trhough-to-output-unmodified ()
  "Test that pb-modeline-output-filter pipes the input remains unmodified when returned"
  (let ((pb-current-server-project (list (cons "setup-dir" "my-dir"))))
    (with-mock
      (mock (pb-set-modeline-status *))
      (should (string= (mock-pb-build-success-message) (pb-modeline-output-filter (mock-pb-build-success-message)))))))

(ert-deftest test-pb-modeline-output-filter-calls-pb-set-modeline-status-with-expected-arguments ()
  "Test that pb-modeline-output-filter calls pb-set-modeline-status with the expected argument"
  (let ((pb-current-server-project (list (cons "setup-dir" "my-dir"))))
    (with-mock
      (mock (pb-set-modeline-status "  .--.fcrepo-solr-testrunner: 7 / 85.78%  "))
      (pb-modeline-output-filter (mock-pb-build-success-message)))))

(ert-deftest test-pb-server-real-buffer-name-returns-the-expected-real-buffer-name ()
  "Test whether the pb-server-real-buffer-name returns the expected real buffer name"
  (let ((pb-server-buffer-name "FOOBAR"))
    (should (string= "*FOOBAR*" (pb-server-real-buffer-name)))))


(ert-deftest test-pb-server-stop-sets-modeline-as-expected ()
  "Test that pb-server-stop sets the modeline as expected"
  (let ((pybuilder-default-lighter " this-lighter")
        (pb-current-server-project (list "pyBuilder")))
    (with-mock
      (mock (pb-server-run-command *))
      (mock (pb-set-modeline-status " this-lighter"))
      (pb-server-stop))))

(ert-deftest test-pb-server-stop-calls-server-with-quit-command ()
  "Test that pb-server-stop calls the server with the expected quit command"
  (let ((pybuilder-default-lighter " this-lighter")
        (pb-current-server-project (list "pyBuilder")))
    (with-mock
      (mock (pb-server-run-command "quit"))
      (mock (pb-set-modeline-status *))
      (pb-server-stop))))

(ert-deftest test-pb-server-stop-resets-project-info ()
  "Test that pb-server-stop resets the pb-current-server-project variable"
  (let ((pybuilder-default-lighter " this-lighter")
        (pb-current-server-project (list "pyBuilder")))
    (with-mock
      (mock (pb-server-run-command *))
      (mock (pb-set-modeline-status *))
      (pb-server-stop)
      (should (eq pb-current-server-project nil)))))

(ert-deftest test-pb-server-stop-does-not-raise-if-no-server-is-running ()
  "Test that pb-server-stop does not raise if no server is running"
  (let ((pybuilder-default-lighter " this-lighter"))
    (pb-server-stop)
    (should (eq pb-current-server-project nil))))

(ert-deftest pb-server-run-command-with-no-arguments-calls-comint-as-expected ()
  "Test that pb-server-run-command calls comint with the expected string when no arguments are supplied"
  (save-window-excursion
    (let ((pb-current-server-project (list "pyBuilder")))
      (flet ((pb-server-real-buffer-name () "*python-build-buffer*"))
        (unwind-protect
            (with-mock
              (get-buffer-create "*python-build-buffer*")
              (mock (comint-send-string "*python-build-buffer*" "command\n" *))
              (pb-server-run-command "command"))
          (kill-buffer (pb-server-real-buffer-name)))))))

(ert-deftest pb-server-run-command-with-arguments-calls-comint-as-expected ()
  "Test that pb-server-run-command calls comint with the expected string when arguments are supplied"
  (save-window-excursion
    (let ((pb-current-server-project (list "pyBuilder")))
      (flet ((pb-server-real-buffer-name () "*python-build-buffer*"))
        (unwind-protect
            (with-mock
              (get-buffer-create "*python-build-buffer*")
              (mock (comint-send-string "*python-build-buffer*" "command foo bar\n" *))
              (pb-server-run-command "command" "foo" "bar"))
          (kill-buffer (pb-server-real-buffer-name)))))))

(ert-deftest pb-server-run-command-raises-error-if-no-server-is-running ()
  "Test that pb-server-run-command raises error if no server is running"
  (should-error (pb-server-run-command "command")))

(provide 'test-pb-server)


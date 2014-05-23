(defun pb-fix-org-setup-file () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/example-project/setup.py"))
(defun pb-fix-org-src-file () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/example-project/example.py"))
(defun pb-fix-org-src-file-2 () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/example-project/example_two.py"))
(defun pb-fix-org-test-file () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/example-project/test_example.py"))
(defun pb-fix-org-data-dir () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/"))
(defun pb-fix-example-coverage () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/example-coverage.xml"))
(defun pb-fix-example-xml () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/test.xml"))
(defun pb-fix-example-compile-file () (concat (expand-file-name pybuilder-home) "/elisp/tests/data/compile-file"))

 
(defun pb-fix-temp-dir (body &optional prefix suffix)
  "Fixture providing temporary dir"
  (let ((temp-dir (file-name-as-directory (make-temp-file prefix t suffix))))
    (unwind-protect
        (funcall body temp-dir)
      (delete-directory temp-dir t))))


(defun pb-fix-python-project (body install-dir)
  "Fixture Providing small python project"
  (let ((files))

    (setq files (append files (list (cons 'project-dir (concat (file-name-as-directory install-dir) "example-project")))))
    (setq files (append files (list (cons 'src-dir (concat (file-name-as-directory (cdr (assoc 'project-dir files))) "src")))))
    (setq files (append files (list (cons 'package-dir (concat (file-name-as-directory (cdr (assoc 'src-dir files))) "example_project")))))
    (setq files (append files (list (cons 'test-dir (concat (file-name-as-directory (cdr (assoc 'package-dir files))) "tests")))))
    (setq files (append files (list (cons 'setup-file (concat (file-name-as-directory (cdr (assoc 'project-dir files))) "setup.py")))))
    (setq files (append files (list (cons 'src-file (concat (file-name-as-directory (cdr (assoc 'package-dir files))) "example.py")))))
    (setq files (append files (list (cons 'src-file-2 (concat (file-name-as-directory (cdr (assoc 'package-dir files))) "example_two.py")))))
    (setq files (append files (list (cons 'test-file (concat (file-name-as-directory (cdr (assoc 'test-dir files))) "test_example.py")))))

    (make-directory (cdr (assoc 'project-dir files)))
    (make-directory (cdr (assoc 'src-dir files)))
    (make-directory (cdr (assoc 'package-dir files)))
    (make-directory (cdr (assoc 'test-dir files)))

    (write-region "" nil (concat (file-name-as-directory (cdr (assoc 'package-dir files))) "__init__.py"))
    (write-region "" nil (concat (file-name-as-directory (cdr (assoc 'test-dir files))) "__init__.py"))

    (copy-file (pb-fix-org-setup-file) (cdr (assoc 'setup-file files)))
    (copy-file (pb-fix-org-src-file) (cdr (assoc 'src-file files)))
    (copy-file (pb-fix-org-src-file-2) (cdr (assoc 'src-file-2 files)))
    (copy-file (pb-fix-org-test-file) (cdr (assoc 'test-file files)))
    
    (funcall body files)))

(defun pb-fix-temp-buffer (body file)
  "Fixture providing temp buffer"
  (save-window-excursion
    (find-file file)
    (switch-to-buffer (file-name-nondirectory file))
    (goto-char (point-min))
    (unwind-protect
        (funcall body)
      (progn (sleep-for 0 200)
             (kill-buffer (file-name-nondirectory file))
             ))))

(provide 'pb-fixtures)

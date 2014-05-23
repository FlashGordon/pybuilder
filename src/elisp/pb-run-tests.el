;;; pb-run-tests.el --- Runs python unittests
;;
;; Filename:pb-run-tests.el
;; Description: Runs python unittests
;; Author: shm
;;
;;; Commentary:
;;
;;  Contains functions for testing def/class/module/package under
;;  point.
;;
;;  Interactive functions:
;;  * pybuilder-run-all
;;  * pybuilder-run-pkg
;;  * pybuilder-run-cls
;;  * pybuilder-run-def
;;  * pybuilder-compile-all
;;  * pybuilder-compile-pkg
;;  * pybuilder-compile-cls
;;  * pybuilder-compile-def
;;
;;  The functions runs the unitest(s) under point. The compile
;;  functions runs the build result through the compile module.
;;
;;; Code:
(require 'python-mode)
(require 'base)
(require 'pb-setup-file)
(require 'pb-scripts)
(require 'pb-output-filters)

;;;   --- Interactive Functions ---

(defun pybuilder-run-all ()
  "Run all tests in project under point"
  (interactive) (pb-run-test-under-point "provides"))

(defun pybuilder-run-pkg ()
  "Run all tests in package under point"
  (interactive) (pb-run-test-under-point "package"))

(defun pybuilder-run-cls ()
  "Run all tests in class under point"
  (interactive) (pb-run-test-under-point "class"))

(defun pybuilder-run-def ()
  "Run test under point"
  (interactive) (pb-run-test-under-point "def"))

(defun pybuilder-compile-all ()
  "Run all tests in project under point and use compile to display result"
  (interactive) (pb-run-test-under-point "provides" t))

(defun pybuilder-compile-pkg ()
  "Run all tests in package under point and use compile to display result"
  (interactive) (pb-run-test-under-point "package" t))

(defun pybuilder-compile-cls ()
  "Run all tests in class under point and use compile to display result"
  (interactive) (pb-run-test-under-point "class" t))

(defun pybuilder-compile-def ()
  "Run test under point and use compile to display result"
  (interactive) (pb-run-test-under-point "def" t))

;;;  private
(defun pb-name-of-def-or-class (type)
  "name of def or class. TYPE can be 'class' or 'def'"

  (let ((item) (found))

    (save-excursion
      (if (string= "def" type)
          (setq item (py-beginning-of-def))
        (setq item (py-beginning-of-class)))
      (when item
        (setq found t)
        (re-search-forward (concat " *" type  " *"))
        (push-mark)
        (re-search-forward "\(")
        (setq item (chomp (buffer-substring (- (mark) 1) (- (point) 1))))))

    (unless found
      (error "Could not find %s" type))
    item))

(defun pb-module-path (package-dir)
  "builds the module path from PACKAGE-DIR to current buffer"
  (let ((path (replace-regexp-in-string "/" "." (substring (expand-file-name buffer-file-name) (length package-dir) -3))))
    (if (= (string-match "\\." path) 0)
        (substring path 1)
      path)))

(defun pb-paths-to-test-under-point ()
  "Creates module paths to test under point, and returns project and paths"
  (let ((project)
        (setup-file (pb-setup-file-find-setup-file)))
    (when setup-file
      (let* ((project (pb-setup-file-create-project-info setup-file))
             (provides (cdr (assoc "provides" project)))
             (package (pb-module-path (cdr (assoc "package-dir" project))))
             (class (concat package ":" (condition-case nil
                                            (pb-name-of-def-or-class "class")
                                          (error nil))))
             (def (concat class "." (condition-case nil
                                        (pb-name-of-def-or-class "def")
                                      (error nil)))))
        (list project provides package class def)))))

(defun pb-run-test-under-point (path-type &optional compile)
  "Run test under point"
  (let* ((project-and-paths (pb-paths-to-test-under-point))

         (setup-dir (cdr (assoc "setup-dir" (nth 0 project-and-paths))))
         (path (cond ((string= "provides" path-type) (nth 1 project-and-paths))
                     ((string= "package" path-type) (nth 2 project-and-paths))
                     ((string= "class" path-type) (nth 3 project-and-paths))
                     ((string= "def" path-type) (nth 4 project-and-paths)))))
    (message (pb-output-filter-create-test-output 
              (shell-command-to-string (concat "python " (pb-pybuilder-script) " -p emacs --test " path " " setup-dir))))
    (when compile
      (compile (concat (pb-compiler-script) " " (expand-file-name ".compile" setup-dir))))))

(provide 'pb-run-tests)
;;; pb-run-tests.el ends here
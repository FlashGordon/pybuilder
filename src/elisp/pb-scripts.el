;;; pb-scripts.el --- Python scripts used by pybuilder
;;
;; Filename: pb-scripts.el
;; Description: Python scripts used by pybuilder mode
;; Author: shm
;;
;;; Commentary:
;;
;;  Part of pybuilder-mode.
;;
;;; Code:

(defun pb-pybuilder-script () (pb-check-compiled-path (concat (expand-file-name pybuilder-home) "/python/pybuilder.py")))
(defun pb-compiler-script () (pb-check-compiled-path (concat (expand-file-name pybuilder-home) "/python/pycompiler.py")))
(defun pb-setup-file-get-setup-info-script () (pb-check-compiled-path (concat (expand-file-name pybuilder-home) "/python/setup_file_print_setup_info.py")))
(defun pyfly-check-script () (pb-check-compiled-path (concat (expand-file-name pybuilder-home) "/python/pychecker.py")))

(defun pb-check-compiled-path (file-path)
  (if (file-exists-p (concat file-path "c"))
      (concat file-path "c")
    file-path))


;;(message (pb-pybuilder-script))
(provide 'pb-scripts)
;;; pb-scripts.el ends here

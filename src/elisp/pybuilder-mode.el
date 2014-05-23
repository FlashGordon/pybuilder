;;; pybuilder-mode.el --- Unittesting and test-coverage tool
;;
;; Filename:fringe-container.el
;; Description: Unittesting and test-coverage tool
;; Author: shm
;; Version: 0.1
;; Keywords: python unittest code-coverage
;;
;;; Commentary:
;;
;;  Provides functionality for developing python projects.
;;
;;  Usage:
;;
;;    ;; The pybuilder-format-script can be set if external script should be used, otherwise default is used.
;;    (defvar pybuilder-format-script "/home/shm/repos/git/elisp-devel/pybuilder/src/shell/dbc-format-python")
;;
;;    (add-to-list 'load-path "/home/user/.emacs.d/pybuilder/")
;;    (defvar pybuilder-home "/home/user/.emacs.d/pybuilder/")
;;    (require 'pybuilder-mode)
;;
;;; Code:

(require 'cl)
(require 'pb-faces)
(require 'pb-setup-file)
(require 'pb-run-tests)
(require 'pb-server)
(require 'pb-coverage)
(require 'pyfly)


(defgroup pybuilder nil
  "Python project builder framework"
  :group 'python)

(defconst pybuilder-default-lighter " pybuilder " "default lighter for pybuilder")


(defun pybuilder-turn-on-mode ()
  (pybuilder-mode 1))


(define-minor-mode pybuilder-mode
  "Pybuilder mode.

Provides functionality for developing python projects.

The minor mode starts a flymake-mode, that marks up pep8 and
pyflakes results in python buffers.

The mode provides several functions to run unittests under
point. The results can also be run through the compile module for
easy stacktrace access.

A build server is also provided. When started, the project under
point will be build continously in the background, and the result
displayed in the minibuffer.

When the build server is active, the coverage tool can be
activated providing markup in each active project buffer.

Installation:
 `pybuilder-home'
 `pybuilder-version'
 `pb-server-build-timer'

Key bindings:
   \\{pybuilder-mode-map}
"
  :init-value 1
  :global t
  :lighter pybuilder-default-lighter
  :keymap

  '(("\C-ca" . pybuilder-compile-def)
    ("\C-cs" . pybuilder-compile-cls)
    ("\C-cd" . pybuilder-compile-pkg)
    ("\C-cf" . pybuilder-compile-all)
    ("\C-cz" . pybuilder-run-def)
    ("\C-cx" . pybuilder-run-cls)
    ("\C-cc" . pybuilder-run-pkg)
    ("\C-cv" . pybuilder-run-all)

    ("\C-ce" . pybuilder-quick-compile)
    ("\C-ct" . pybuilder-toggle-server)
    ("\C-cr" . pybuilder-coverage-toggle)

    ("\C-c;" . pyfly-toggle) ;; f* button?
    ("\C-cj" . pyfly-at-line)
    ("\C-ck" . pyfly-prev)
    ("\C-cl" . pyfly-next)
    ("\C-co" . pyfly-format-buffer))

  :group 'pybuilder

  (when pb-current-server-project
    (pb-server-stop)))

(define-globalized-minor-mode global-pybuilder-mode
  pybuilder-mode pybuilder-turn-on-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc functions

;; (defun pb-format-script ()
;;   "Finds format script"
;;   (if (boundp 'pybuilder-format-script)
;;       pybuilder-format-script
;;     (concat (expand-file-name pybuilder-home) "python/dbc-format-python")))

;; (defun pybuilder-format-buffer ()
;;   "formats current buffer"
;;   (interactive)
;;   (let ((mark (point))
;;         (formatted-content (replace-regexp-in-string 
;;                             "\ndone$" ""
;;                             (shell-command-to-string
;;                              (concat "bash " (pb-format-script) " --stdout " (buffer-file-name))))))
;;     (kill-region (point-min) (point-max))
;;     (insert formatted-content)
;;     (when (< mark (point-max))
;;       (goto-char mark))
;;     (message "buffer formatted")))

;;;;;;;;
;; Setup
;;
;; TODO: Move to .emacs ?

;; setup flymake
(when (load "flymake" t)
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyfly-init)))

;; setup formatter and save hook
;; (add-hook 'python-mode-hook
;;           (lambda()
;;             (add-hook 'before-save-hook
;;                       'pybuilder-format-buffer nil t)))
;;
;;;;;;;;

(provide 'pybuilder-mode)
;;; pybuilder-mode.el ends here

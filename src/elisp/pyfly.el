;;; pyfly.el --- flymake functions for python code
;;
;; Filename: pyfly.el
;; Description: flymake functions for python code
;;
;;; Commentary:
;;
;; Marks up python buffers according to pep8 and pyflakes output.
;;
;; Part of pybuilder-mode.
;;
;;; Code:

(require 'pb-scripts)

(defun flymake-pyfly-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list (pyfly-check-script) (list local-file))))

(defun pyfly-toggle ()
  "toggles flymake in python mode"
  (interactive)
  (if flymake-mode
      (progn
        (message "Flymake deactivated")
        (flymake-mode -1))
    (progn
      (message "Flymake activated")
      (flymake-mode 1))))

(defun pyfly-next()
  "Goes to next error/warning and displays error message in minibuffer"
  (interactive)
  (flymake-goto-next-error)
  (message "%s" (pyfly-print-err-at-line)))

(defun pyfly-prev()
  "Goes to previous error/warning and displays error message in minibuffer"
  (interactive)
  (flymake-goto-prev-error)
  (message "%s" (pyfly-print-err-at-line)))

(defun pyfly-at-line()
  "Prints error/warning at line if any"
  (interactive)
  (let ((answer (pyfly-print-err-at-line)))
    (if answer
        (message "%s" answer))))

(defun pyfly-print-err-at-line ()
  "Returns string comprised of the errors/warnings for this line"
  (let ((index 0)(result nil))
    (while (< index (length flymake-err-info))
      (if (= (line-number-at-pos)(car (nth index flymake-err-info)))
          (let ((match (car (cdr (nth index flymake-err-info))))(full-err-mesg)(reduced-err-mesg "")(warning nil))
            (dolist (elem match)
              (setq full-err-mesg (cddr (split-string (elt elem 4) " ")))
              (if (string-match "Warning" (car (split-string (elt elem 4) " ")))
                  (setq warning t))
              (setq reduced-err-mesg "")
              (while full-err-mesg
                (if warning
                    (setq reduced-err-mesg (propertize (concat reduced-err-mesg " " (car full-err-mesg)) 'face 'flymake-warnline))
                  (setq reduced-err-mesg (propertize (concat reduced-err-mesg " " (car full-err-mesg)) 'face 'flymake-errline)))
                (setq full-err-mesg (cdr full-err-mesg)))
              (setq result (concat result " / " (substring reduced-err-mesg 1))))
            (setq index (length flymake-err-info))))
      (setq index (1+ index)))
    (if result
        (substring result 3)
      nil)))

(defun pyfly-format-buffer ()
  "formats current buffer"
  (interactive)
  (let ((mark (point))
        (formatted-content (shell-command-to-string
                            (concat "python " (pyfly-check-script) " -f " (buffer-file-name)))))

    (kill-region (point-min) (point-max))
    (insert formatted-content)

    (when (< mark (point-max))
      (goto-char mark))
    (message "buffer formatted")))


(provide 'pyfly)
;;; pyfly.el ends here

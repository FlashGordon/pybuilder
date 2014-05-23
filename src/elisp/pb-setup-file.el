;;; pb-setup-file.el --- Provides functions for getting data from python setup file
;;
;; Filename: pb-setup-file.el
;; Description: Provides functions for getting data from setup file
;; Author: shm
;;
;;; Commentary:
;;
;;  Provides functions to handle python setup file.
;;
;;  pb-setup-file is part of pybuilder-mode.
;;
;;; Code:

(require 'pb-scripts)
(require 'base)

(defun pb-setup-file-find-setup-file (&optional dir)
  "searches path recursively for setup.py file and return the
   path or nil if no file is found. If DIR is non-nil the path to
   the dir containing the setup file is returned"
  (let ((folder (file-name-directory (expand-file-name buffer-file-name))))
    (while (and (not (string= "" folder))
                (not (member "setup.py" (directory-files folder))))
      (setq folder (mapconcat 'identity (butlast (split-string folder "/") 1) "/" )))
    (if (string= "" folder)
        (error "Could not find setup file for buffer '%s'" buffer-file-name)
      (if dir
          (file-name-directory (expand-file-name "setup.py" folder))
        (expand-file-name "setup.py" folder)))))

(defun pb-setup-file-create-project-info (setup-file)
  "creates project info list based on SETUP-FILE"
  (if (file-exists-p setup-file)
      (with-temp-buffer
        (shell-command (concat "python " (pb-setup-file-get-setup-info-script) " " setup-file) 1)
        (goto-char (point-min))
        (let ((status)(key-value-list))
          (setq status (re-search-forward "\\(.*\\):"))
          (if (string= "OK" (match-string 1))
              (while (re-search-forward "\\(.*\\)=\\(.*\\)" nil t )
                (setq key-value-list (cons (cons (match-string 1)(match-string 2)) key-value-list)))
            (error "error reported from python script '%s' error %s" (pb-setup-file-get-setup-info-script) (match-string 1)))
          key-value-list))
    (error "Could not find setup file '%s'" setup-file)))

(defun pb-setup-file-call-setup (setup-file args)
  "Calls the SETUP-FILE with ARGS"
  (let* ((setup-dir (file-name-directory setup-file))
         (cmd (concat "( cd " setup-dir (pb-setup-file-pyenv-string setup-dir) " && "
                      (mapconcat 'identity (append '("python" "setup.py") args) " ") " && cd - > /dev/null )")))
    (chomp (shell-command-to-string cmd))))

(defun pb-setup-file-pyenv-string (setup-dir)
  "constructs source line if pyenv file is present"
  (if (member "pyenv" (directory-files setup-dir))
      " && source pyenv > /dev/null" ""))

(provide 'pb-setup-file)
;;; pb-setup-file.el ends here
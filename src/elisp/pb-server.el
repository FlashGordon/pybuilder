;;; pb-server.el --- Interface for python build server
;;
;; Filename: pb-server.el
;; Description: Interface for python build server
;; Author: shm
;;
;;; Commentary:
;;
;;  Contains methods for starting, stopping and communicating with
;;  python build server, used to build python projects.
;;
;;  Contains the following interactive functions:
;;  * pybuilder-toggle-server
;;  * pybuilder-build
;;  * pybuilder-compile
;;  * pybuilder-quick-compile
;;
;;  pb-server is part of pybuilder-mode.
;;
;;; Code:
(require 'comint)
(require 'base)
(require 'pb-faces)
(require 'pb-scripts)
(require 'pb-setup-file)
(require 'pb-output-filters)
(require 'pb-coverage)

(defconst pb-server-buffer-name "python-build-buffer" "Name of buffer to place server output in")
(defvar pb-server-build-timer "10" "Build-timer for server")
(defvar pb-current-server-project nil "Contains the info about the current server project")

;;;   --- Interactive Functions ---
(defun pybuilder-toggle-server ()
  "Toggles build server on/off"
  (interactive)
  (if pb-current-server-project
      (pb-server-stop)
    (pb-server-start)))

(defun pybuilder-build ()
  "Builds project now"
  (interactive)
  (pb-server-run-command "build"))

(defun pybuilder-compile ()
  "Builds project now, and show result through compile"
  (interactive)
  (add-hook 'comint-output-filter-functions 'pb-compile-output-filter)
  (pb-server-run-command "build"))

(defun pybuilder-quick-compile ()
  "show result of last executed build through compile"
  (interactive)
  (if pb-current-server-project
      (pb-server-compile (cdr (assoc "setup-dir" pb-current-server-project)))
    (message "No server running")))

;;;  private
(defun pb-set-modeline-status ( mesg )
  "Display MESG in pybuilder part of modeline"
  (setq minor-mode-alist
        (cons (list 'pybuilder-mode mesg )
              (assq-delete-all 'pybuilder-mode minor-mode-alist))))

(defun pb-server-compile (setup-dir)
  "Calls compile with build output found in .compile file in SETUP-DIR"
  (let ((compile-file (expand-file-name ".compile" setup-dir)))
    (if (file-exists-p compile-file)
        (compile (concat (pb-compiler-script) " " compile-file))
      (error "Could not find .compile file in path '%s'" setup-dir))))

(defun pb-compile-output-filter (org-output)
  "preoutput filter for comint hook. used when building with compile. the hook removes itself when called"
  (pb-server-compile (cdr (assoc "setup-dir" pb-current-server-project)))
  (remove-hook 'comint-output-filter-functions 'pb-compile-output-filter)
  org-output)

(defun pb-modeline-output-filter (org-output)
  "preoutput filter for comint hook. Used to to update pybuilder modeline"
  (pb-set-modeline-status (pb-output-filter-create-build-output org-output))
  org-output)

(defun pb-server-real-buffer-name ()
  "Creates actual buffer name"
  (concat "*" pb-server-buffer-name "*"))

(defun pb-server-start (&optional setup-file)
  "starts server building project poitined to by SETUP-FILE"
  (when pb-current-server-project
    (message "stopping already active server")
    (pb-server-stop))

  (unless setup-file
    (setq setup-file (pb-setup-file-find-setup-file)))
  
  (add-hook 'comint-preoutput-filter-functions 'pb-modeline-output-filter)
  (setq pb-current-server-project (pb-setup-file-create-project-info setup-file))
  (message "Starting build-server - project '%s'" (cdr (assoc "name" pb-current-server-project)))
  (pb-set-modeline-status (format "  .--.%s  " (cdr (assoc "name" pb-current-server-project))))
  (make-comint pb-server-buffer-name "python" nil (pb-pybuilder-script)
               (cdr (assoc "setup-dir" pb-current-server-project)) "-i" pb-server-build-timer "-p" "emacs"))

(defun pb-server-stop ()
  "Stops build server"
  (if pb-current-server-project
      (progn

        (when (member 'pb-update-coverage-fringes-filter comint-preoutput-filter-functions)
          (pb-coverage-turn-off))

        (condition-case nil
            (pb-server-run-command "quit")
          (error nil))

        (pb-set-modeline-status pybuilder-default-lighter)
        (message "Stopped build-server - project '%s'" (cdr (assoc "name" pb-current-server-project)))
        (setq pb-current-server-project nil))

    (message "No server running")))

(defun pb-server-run-command (command &rest args)
  "Runs COMMAND in server"
  (if pb-current-server-project
      (let ((command-string (chomp (concat command " " (mapconcat 'identity args " ")))))
        (with-current-buffer (pb-server-real-buffer-name)
          (comint-send-string (pb-server-real-buffer-name) (concat command-string "\n"))))
    (error "No server running")))

(provide 'pb-server)
;;; pb-server ends here
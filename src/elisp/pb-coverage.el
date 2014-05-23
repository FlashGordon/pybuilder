;;; pb-coverage.el --- Creates fringes according to coverage-report
;;
;; Filename: pb-coverage-report.el
;; Description: Creates fringes according to coverage-report
;; Author: shm
;;
;;; Commentary:
;;
;; Provides the interactive function pybuilder-coverage-toggle, which
;; adds the function pb-update-coverage-fringes-filter to comint
;; hook. This reads the projects coverage.xml file and updates
;; coverage fringes. This is designed to be triggered by pybuilder
;; periodical builds.
;;
;; Provide one interactive function:
;; * pybuilder-coverage-toggle
;;
;; Part of pybuilder-mode.
;;
;;; Code:

;; TODO:: Unittest module
(require 'pb-faces)
(require 'pb-coverage-report)
(require 'fringe-container)

(require 'fringe-helper)

;;;   --- Interactive Functions ---

(defun pybuilder-coverage-toggle ()
  "Toggle coverage"
  (interactive)
  (if (member 'pb-update-coverage-fringes-filter comint-preoutput-filter-functions)
      (pb-coverage-turn-off)
    (pb-coverage-turn-on)))

;;;  --- Bitmaps ---

(fringe-helper-define 'success-bitmap nil
                      ".XX....XX"
                      ".XX...XX."
                      ".XX...XX."
                      ".XX..XX.."
                      ".XX.XX..."
                      ".XXXX...."
                      ".XXX....."
                      ".XX......")

(fringe-helper-define 'error-bitmap nil
                      "XX.....XX"
                      ".XX...XX."
                      "..XX.XX.."
                      "...XXX..."
                      "...XXX..."
                      "..XX.XX.."
                      ".XX...XX."
                      "XX.....XX")

;;;  private

(defun pb-coverage-turn-on ()
  "Turns on coverage"
  (message "Starting coverage")
  (when (not pb-current-server-project)
    (error "Can only turn coverage on while build server is running"))
  (add-hook 'comint-preoutput-filter-functions 'pb-update-coverage-fringes-filter))

(defun pb-coverage-turn-off ()
  "Turns off coverage"
  (message "Stopping coverage")
  (fringe-container-remove-all-fringes)
  (remove-hook 'comint-preoutput-filter-functions 'pb-update-coverage-fringes-filter))

(defun pb-get-face (status)
  "Return face according to STATUS"
  (cond ((= 0 status) '(pb-error-face error-bitmap))
        ((= 1 status) '(pb-success-face success-bitmap))
        (t (error "Unknown status: %s" status))))

(defun pb-create-coverage-report ()
  "Create coverage report for active project buffers"
  (when (not pb-current-server-project)
    (error "Build server needs to be running for report to be created"))

  (let ((setup-dir (file-name-as-directory (cdr (assoc "setup-dir" pb-current-server-project)))))
    (pb-transform-line-numbers-to-positions
     (pb-filter-coverage-tests
      (pb-parse-coverage-xml
       (pb-read-xml (concat setup-dir "coverage.xml")))
      setup-dir)
     'pb-get-face)))

(defun pb-update-coverage-fringes-filter (output)
  "Comint hook for updating coverage fringes"
  (when fringe-container-fringes
    (fringe-container-remove-all-fringes))

  (mapc '(lambda (x)
           (fringe-container-add-fringes-to-buffer (file-name-nondirectory (nth 0 x)) (nth 1 x)))
        (pb-create-coverage-report))
  output)

(provide 'pb-coverage)
;;; pb-coverage.el ends here
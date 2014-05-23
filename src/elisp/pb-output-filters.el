;;; pb-output-filters.el --- Output filters for pybuilder-mode
;;
;; Filename: pb-output-filters.el
;; Description: Output filters for pybuilder
;; Author: shm
;;
;;; Commentary:
;;
;;  Output filters designed to display test and pybuilder messages.
;;
;;  Part of pybuilder-mode.
;;
;;; Code:

(require 'pb-faces)
(require 'base)

(defun pb-output-filter-create-test-output (input)
  "creates and returns test output string from INPUT"
  (let ((data (pb-output-filter-parse-server-output input)))
    (if (string-match "[emacs.test.build-result]" (nth 0 data))
        (format "%s %s: %s test%s - %s"
                (pb-output-filter-propertize-string ">" (nth 4 data) "minibuffer")
                (nth 1 data)
                (nth 3 data)
                (if (string-match "1" (nth 3 data)) "" "s" )
                (pb-output-filter-parse-python-test-output (nth 7 data)))
      (error "Unknown build identifier %s" (nth 0 data)))))

(defun pb-output-filter-create-build-output (input &optional modeline)
  "creates build output string from INPUT"
  (let ((data (pb-output-filter-parse-server-output input)))
    (cond ((string-equal "[emacs.build.build-start]" (nth 0 data))
           (let ((modeline-face)
                 (build-indicator ".b." )
                 (modeline (nth 1 (assoc 'pybuilder-mode minor-mode-alist)))
                 )
             (unless modeline (setq modeline ".--."))
             (setq modeline-face (get-text-property 1 'face modeline ))
             (when modeline-face
               (setq build-indicator (propertize build-indicator 'face modeline-face)))
             (string-match "\\( *\\)\\(\\..+?\\.\\)\\(.*\\)" modeline)
             (format "%s%s%s" (match-string 1 modeline)
                     build-indicator
                     (match-string 3 modeline))))
          ((string-equal "[emacs.build.build-result]" (nth 0 data))
           (let ((out))
             (if (string-equal "0" (nth 4 data))
                 (setq out (format "  .--.%s: %s / %s  "  (nth 1 data) (nth 3 data) (nth 7 data)))
               (setq out (format "  .--.%s: %s  "  (nth 1 data) (pb-output-filter-parse-python-test-output (nth 7 data)))))
             (pb-output-filter-propertize-string out (nth 4 data) "modeline")))
          (t (error "Unknown build identifier %s" (nth 0 data))))))

;;;  private
(defun pb-output-filter-propertize-string (string status type)
  "propertize STRING according to STATUS and TYPE"
  (cond ((string-equal "0" status)
         (if (string-equal "modeline" type)
             (propertize string 'face 'pb-modeline-success-face)
           (propertize string 'face 'pb-success-face)))

        ((string-equal "1" status)
         (if (string-equal "modeline" type)
             (propertize string 'face 'pb-modeline-failure-face)
           (propertize string 'face 'pb-failure-face)))

        ((string-equal "2" status)
         (if (string-equal "modeline" type)
             (propertize string 'face 'pb-modeline-error-face)
           (propertize string 'face 'pb-error-face)))
        (t (error "Unknown status type '%s'" status))))

(defun pb-output-filter-parse-python-test-output (data)
  "parses test output"
  (if (string-match "OK" data)
      data
    (progn (string-match "FAILED (\\(.*\\))" data)
           (match-string 1 data))))

(defun pb-output-filter-parse-server-output (raw-output)
  "parses python test output"
  (mapcar '(lambda (x) (chomp x)) (split-string raw-output ",")))


(provide 'pb-output-filters)
;;; pb-output-filters.el ends here
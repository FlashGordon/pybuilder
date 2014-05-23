;;; pb-coverage-report.el --- Creates coverage report for pybuilder-mode
;;
;; Filename: pb-coverage-report.el
;; Description: Creates coverage report for pybuilder-mode
;; Author: shm
;;
;;; Commentary:
;;
;; Provides functions for parsing python coverage xml.
;;
;; Part of pybuilder-mode.
;;
;;; Code:

(require 'xml-parse)
(require 'base)

(defun pb-read-xml (file)
  "reads xml from FILE"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (read-xml)))

(defun pb-parse-coverage-xml (xml)
  "Parses coverage result XML, and returns regions"
  (let ((coverage)
        (packages (cdr (assoc "packages" xml))))

    (dolist (package packages)
      (let ((classes (cdr (assoc "classes" package))))

        (dolist (class classes)
          (setq coverage (cons (list (cdr (assoc "filename" (cdr (assoc "class" class))))
                                     (reverse (pb-parse-coverage-lines-xml (cdr (assoc "lines" class)))))
                               coverage)))))
    (if xml
        coverage
      (error "Xml cannot be nil"))))

(defun pb-filter-coverage-tests (coverage-result path-prefix)
  "Filters coverage result so only results with active buffer is left"
  (let ((file-buffer-names (filter (lambda (x) (not (string-match ".*/tests/.*test.*" x))) 
                                   (filter (lambda (x) (if x x)) 
                                           (mapcar (lambda (x) (buffer-file-name x)) (buffer-list))))))
    (filter (lambda (x) (member (expand-file-name (car x) path-prefix) file-buffer-names)) coverage-result)))

(defun pb-transform-line-numbers-to-positions (coverage-result &optional block-coverage-status-func)
  "Transforms line-numbers in COVERAGE-RESULT to positions"
  (mapcar (lambda (entry)
            (pb-transform-buffer entry block-coverage-status-func))
          coverage-result))

;;;  private
(defun pb-parse-coverage-lines-xml (lines)
  "parses lines-xml and return regions"
  (let ((line 0) (hits 0) (last-line 0) (region-start) (regions))

    (defun pb-set-values (&optional set-regions)
      (when set-regions (setq regions (cons (list region-start last-line hits) regions)))
      (setq hits (string-to-number (cdr (assoc "hits" (car line)))))
      (setq region-start (string-to-number (cdr (assoc "number" (car line)))))
      (setq last-line region-start))

    (dolist (line lines)
      (if region-start
          (if (and (block-continues? last-line line)
                   (status-unchanged? hits line))
              (setq last-line (string-to-number (cdr (assoc "number" (car line)))))
            (pb-set-values t))
        (pb-set-values)))

    (unless region-start (setq region-start 0))
    (cons (list region-start last-line hits) regions)))

(defun block-continues? (last-line line)
  "internal function for pb-parse-coverage-lines-xml"
  (= (+ last-line 1) (string-to-number (cdr (assoc "number" (car line))))))

(defun status-unchanged? (hits line)
  "internal function for pb-parse-coverage-lines-xml"
  (let ((line-hits (string-to-number (cdr (assoc "hits" (car line))))))
    (or (and (= hits 0) (= line-hits 0))
        (and (> hits 0) (> line-hits 0)))))

(defun pb-transform-buffer (entry &optional block-coverage-status-func)
  "Transforms positions in buffer"
  (let ((result) (current-line-number 1))

    (save-current-buffer
      (set-buffer (file-name-nondirectory (nth 0 entry)))
      (save-excursion
        (goto-char (point-min))

        (list (nth 0 entry)
              (mapcar
               (lambda (x)
                 (setq result (pb-transform-entry current-line-number x block-coverage-status-func))
                 (setq current-line-number (nth 0 result))
                 (nth 1 result))
               (nth 1 entry)))))))

(defun pb-transform-entry (offset entry &optional block-coverage-status-func)
  "Transform entry from line-numbers to positions"
  (let ((beg)(end)
        (mapping-function '(lambda (x) (list x))))

    (when block-coverage-status-func
      (setq mapping-function block-coverage-status-func))

    (setq offset (pb-move-point offset entry 0))
    (setq beg (line-beginning-position))
    (setq offset (pb-move-point offset entry 1))
    (setq end (line-end-position))
    `(,offset (,beg ,end ,@(funcall mapping-function (nth 2 entry))))))

(defun pb-move-point (offset entry index)
  (forward-line (- (nth index entry) offset))
  (nth index entry))


(provide 'pb-coverage-report)
;;; pb-coverage-report.el ends here
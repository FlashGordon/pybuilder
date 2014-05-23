;;; fringe-container.el --- Provide functions for managing fringes
;;
;; Filename:fringe-container.el
;; Description: Provide functions for managing fringes
;; Author: shm
;;
;;; Commentary:
;;
;; Provide functions for handling fringes in multiple buffers.
;; Default face and bitmap is provided. 
;;
;;  Provides three methods for handling fringes in buffers:
;;  * fringe-container-add-fringes-to-buffer
;;  * fringe-container-remove-fringes-to-buffer
;;  * fringe-container-remove-all-fringes
;;
;;  Examples of usage:
;;
;;   (fringe-container-add-fringes-to-buffer "pybuilder.py" '((400 500 'your-face 'fringe-helper-bitmap)))
;;   (fringe-container-add-fringes-to-buffer "pybuilder.py" '((400 500 'your-face)))
;;   (fringe-container-add-fringes-to-buffer "pybuilder.py" '((400 500)))
;;   (fringe-container-remove-fringes-to-buffer "pybuilder.py")
;;
;;; Code:

(require 'fringe-helper)

(fringe-helper-define 'fringe-container-fringe-bitmap nil
                      "..X......"
                      "..XXX...."
                      "..XXXXX.."
                      "XXXXXXXXX"
                      "XXXXXXXXX"
                      "..XXXXX.."
                      "..XXX...."
                      "..X......")

(defvar fringe-container-fringes nil "Contains all active fringes")

(defun fringe-container-add-fringes-to-buffer (buffername fringe-regions)
  "Add FRINGE-REGIONS to buffer with BUFFERNAME, and add buffer to fringe-container"
  (let ((org-buffer (current-buffer)))
    (when (member buffername (mapcar '(lambda (name) (buffer-name name))
                                     (buffer-list)))
      (set-buffer buffername)
      (fringe-container-add-buffer buffername (fringe-container-create-fringes fringe-regions))
      (set-buffer org-buffer))))


(defun fringe-container-remove-fringes-to-buffer (buffername)
  "remove fringes and buffer with BUFFERNAME from fringe-container"
  (let ((org-buffer (current-buffer)))
    (when (member buffername (mapcar '(lambda (name) (buffer-name name))
                                     (buffer-list)))
      (set-buffer buffername)
      (fringe-container-remove-fringes (cadr (fringe-container-remove-buffer buffername)))
      (set-buffer org-buffer))))

;; TODO: Not Unittested !!! ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun fringe-container-remove-all-fringes ()
  "Remove all fringes from container"
  (mapcar (lambda (x)
            (fringe-container-remove-fringes-to-buffer (nth 0 x)))
          fringe-container-fringes))

;;;  private

(defun fringe-container-add-buffer (buffername fringes)
  (let ((elem (list buffername fringes))
        (old (assoc buffername fringe-container-fringes)))
    (when old
      (fringe-container-remove-fringes (nth 1 old))
      (setq fringe-container-fringes (delq old fringe-container-fringes)))
    (setq fringe-container-fringes (cons elem fringe-container-fringes))
    old))

(defun fringe-container-remove-buffer (buffername)
  (let ((old (assoc buffername fringe-container-fringes)))
    (when old
      (setq fringe-container-fringes (delq old fringe-container-fringes)))
    old))

(defun fringe-container-create-fringes (regions)
  (mapcar '(lambda (region)

             (cond ((= 4 (length region))
                    (fringe-helper-insert-region (nth 0 region) (nth 1 region) (nth 3 region) 'left-fringe (nth 2 region)))
                  ((= 3 (length region))
                    (fringe-helper-insert-region (nth 0 region) (nth 1 region) 'fringe-container-fringe-bitmap 'left-fringe (nth 2 region)))
                   ((= 2 (length region))
                    (fringe-helper-insert-region (nth 0 region) (nth 1 region) 'fringe-container-fringe-bitmap 'left-fringe))))
          regions))

(defun fringe-container-remove-fringes (fringes)
  (mapc '(lambda (fringe) (fringe-helper-remove fringe))
        fringes)
  fringes)

(provide 'fringe-container)
;;; fringe-container.el ends here
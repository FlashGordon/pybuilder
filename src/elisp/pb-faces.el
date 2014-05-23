;;; pb-faces.el --- Faces used by pybuilder mode
;;
;; Filename:pb-faces.el
;; Description: Faces used by pybuilder mode
;; Author: shm
;;
;;; Commentary:
;;  
;;  Contains faces used in pybuilder modeline and minibuffer.
;;
;;  Part of pybuilder-mode.
;;
;;; Code:

(defface pb-modeline-success-face
  '((t (:inherit mode-line-buffer-id
                 :foreground "dark green" )))
  "Face used for displaying a sucessful test result."
  :group 'pybuilder)

(defface pb-modeline-failure-face
  '((t (:inherit mode-line-buffer-id
                 :foreground "dark orange" )))
  "Face used for displaying a failed test result."
  :group 'pybuilder)

(defface pb-modeline-error-face
  '((t (:inherit mode-line-buffer-id
                 :foreground "firebrick" )))
  "Face used for displaying a err test result."
  :group 'pybuilder)

(defface pb-success-face
  '((t (:foreground "dark green" )))
  "Face used for displaying a sucessful test result."
  :group 'pybuilder)

(defface pb-failure-face
  '((t (:foreground "dark orange" )))
  "Face used for displaying a failed test result."
  :group 'pybuilder)

(defface pb-error-face
  '((t (:foreground "firebrick" )))
  "Face used for displaying a err test result."
  :group 'pybuilder)

(provide 'pb-faces)
;;; pb-faces.el ends here

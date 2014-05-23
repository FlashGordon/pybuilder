(require  'ert)
(require 'pb-fixtures)
(require 'fringe-container)
(require 'test-pb-init)

(ert-deftest test-fringe-container-adds-region-as-expected ()
  "Test that a fringe-region is added as expected"

  (pb-fix-temp-buffer
   (lambda ()

         (progn

           (should (not fringe-container-fringes))

           (fringe-container-add-fringes-to-buffer (format "%s" (current-buffer)) '((0 30)))

           (should (= 1 (length fringe-container-fringes)))
           (should (= 1 (length (cadr (assoc (format "%s" (current-buffer)) fringe-container-fringes)))))
           (should (string= (format "%s" (current-buffer)) (caar fringe-container-fringes))))

       (fringe-container-remove-fringes-to-buffer (format "%s" (current-buffer)))
     ) (pb-fix-org-src-file)))


(ert-deftest test-fringe-container-adds-regions-as-expected ()
  "Test that multiple fringe-regions are added as expected"

  (pb-fix-temp-buffer
   (lambda ()

         (progn

           (should (not fringe-container-fringes))

           (fringe-container-add-fringes-to-buffer (format "%s" (current-buffer)) '((0 10) (30 60)))

           (should (= 1 (length fringe-container-fringes)))
           (should (string= (format "%s" (current-buffer)) (caar fringe-container-fringes)))
           (should (= 2 (length (cadr (assoc (format "%s" (current-buffer)) fringe-container-fringes))))))

       (fringe-container-remove-fringes-to-buffer (format "%s" (current-buffer)))
     ) (pb-fix-org-src-file)))


(ert-deftest test-fringe-container-removes-old-entry-if-present ()
  "Test that a fringe-region removed old region if present"
  (pb-fix-temp-buffer
   (lambda ()

         (progn

           (should (not fringe-container-fringes))

           (fringe-container-add-fringes-to-buffer (format "%s" (current-buffer)) '((2 10)))

           (should (= 1 (length fringe-container-fringes)))
           (should (= 1 (length (cadr (assoc (format "%s" (current-buffer)) fringe-container-fringes)))))
           (should (string= (format "%s" (current-buffer)) (caar fringe-container-fringes)))

           (fringe-container-add-fringes-to-buffer (format "%s" (current-buffer)) '((30 60)))

           (should (= 1 (length fringe-container-fringes)))
           (should (string= (format "%s" (current-buffer)) (caar fringe-container-fringes)))
           (should (= 1 (length (cadr (assoc (format "%s" (current-buffer)) fringe-container-fringes)))))

       (fringe-container-remove-fringes-to-buffer (format "%s" (current-buffer))))
     ) (pb-fix-org-src-file)))


(ert-deftest test-fringe-container-removes-region-as-expected ()
  "Test that a fringe-region is removed as expected"

  (pb-fix-temp-buffer
   (lambda ()

         (progn

           (fringe-container-add-fringes-to-buffer (format "%s" (current-buffer)) '((0 30)))
           (should (= 1 (length fringe-container-fringes))))

       (fringe-container-remove-fringes-to-buffer (format "%s" (current-buffer)))

     (should (not fringe-container-fringes))

     ) (pb-fix-org-src-file)))


(provide 'test-fringe-container)
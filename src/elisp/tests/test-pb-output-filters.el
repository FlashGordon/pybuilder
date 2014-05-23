(require 'ert)
(require 'pb-faces)
(require 'pb-output-filters)
(require 'el-mock)


(defun mock-pb-modeline-default () " pybuilder")
(defun mock-pb-modeline-success () (propertize "  .--.fcrepo-solr-testrunner: 7 / 85.78%  " 'face 'pb-modeline-success-face))
(defun mock-pb-modeline-failure () (propertize "  .--.fcrepo-solr-testrunner: failures=1  " 'face 'pb-modeline-failure-face))
(defun mock-pb-modeline-error () (propertize "  .--.fcrepo-solr-testrunner: errors=1  " 'face 'pb-modeline-error-face))

(defun mock-pb-test-success-message () "[emacs.test.build-result],fcrepo-solr-testrunner, 0.1, 7, 0, OK, 0.019, OK")
(defun mock-pb-test-failure-message () "[emacs.test.build-result],fcrepo-solr-testrunner, 0.1, 7, 1, FAILED, 0.019, FAILED (failures=1)")
(defun mock-pb-test-error-message () "[emacs.test.build-result],fcrepo-solr-testrunner, 0.1, 1, 2, FAILED, 0.001, FAILED (errors=1)")

(defun mock-pb-build-start-build-message () "[emacs.build.build-start]")
(defun mock-pb-build-success-message () "[emacs.build.build-result],fcrepo-solr-testrunner, 0.1, 7, 0, OK, 0.246, 85.78%")
(defun mock-pb-build-failure-message () "[emacs.build.build-result],fcrepo-solr-testrunner, 0.1, 7, 1, FAILED, 0.247, FAILED (failures=1)")
(defun mock-pb-build-error-message () "[emacs.build.build-result],fcrepo-solr-testrunner, 0.1, 7, 2, FAILED, 0.249, FAILED (errors=1)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest test-pb-output-filter-propertize-string-expected-property-used-when-status-is-0-and-type-is-modeline ()
  "Test that filter-propertize-string uses the expected property when status is 0 and type is modeline"
  (should (eq 'pb-modeline-success-face
              (get-text-property 1 'face (pb-output-filter-propertize-string "string" "0" "modeline")))))

(ert-deftest test-pb-output-filter-propertize-string-expected-property-used-when-status-is-0-and-type-is-not-modeline ()
  "Test that filter-propertize-string uses the expected property when status is 0 and type is not modeline"
  (should (eq 'pb-success-face
              (get-text-property 1 'face (pb-output-filter-propertize-string "string" "0" "basic")))))

(ert-deftest test-pb-output-filter-propertize-string-expected-property-used-when-status-is-1-and-type-is-modeline ()
  "Test that filter-propertize-string uses the expected property when status is 1 and type is modeline"
  (should (eq 'pb-modeline-failure-face
              (get-text-property 1 'face (pb-output-filter-propertize-string "string" "1" "modeline")))))

(ert-deftest test-pb-output-filter-propertize-string-expected-property-used-when-status-is-1-and-type-is-not-modeline ()
  "Test that filter-propertize-string uses the expected property when status is 1 and type is not modeline"
  (should (eq 'pb-failure-face
              (get-text-property 1 'face (pb-output-filter-propertize-string "string" "1" "basic")))))

(ert-deftest test-pb-output-filter-propertize-string-expected-property-used-when-status-is-2-and-type-is-modeline ()
  "Test that filter-propertize-string uses the expected property when status is 2 and type is modeline"
  (should (eq 'pb-modeline-error-face
              (get-text-property 1 'face (pb-output-filter-propertize-string "string" "2" "modeline")))))

(ert-deftest test-pb-output-filter-propertize-string-expected-property-used-when-status-is-2-and-type-is-not-modeline ()
  "Test that filter-propertize-string uses the expected property when status is 2 and type is not modeline"
  (should (eq 'pb-error-face
              (get-text-property 1 'face (pb-output-filter-propertize-string "string" "2" "basic")))))

(ert-deftest test-pb-output-filter-propertize-string-raises-error-on-unknown-status ()
  "Test that an error is raised if filter-propertize-string is given an unknown status"
  (should-error (pb-output-filter-propertize-string "string" "3" "basic")))

(ert-deftest test-pb-output-filter-create-test-output-creates-expected-output-on-success-input ()
  "Test that pb-output-filter-create-test-output creates the expected output when given a input signifying build success"
  (let ((result (pb-output-filter-create-test-output (mock-pb-test-success-message))))
    (should (string= "> fcrepo-solr-testrunner: 7 tests - OK" result))
    (should (eq 'pb-success-face (get-text-property 0 'face result)))))

(ert-deftest test-pb-output-filter-create-test-output-creates-expected-output-on-failure-input ()
  "Test that pb-output-filter-create-test-output creates the expected output when given a input signifying build failure"
  (let ((result (pb-output-filter-create-test-output (mock-pb-test-failure-message))))
    (should (string= "> fcrepo-solr-testrunner: 7 tests - failures=1" result))
    (should (eq 'pb-failure-face (get-text-property 0 'face result)))))

(ert-deftest test-pb-output-filter-create-test-output-creates-expected-output-on-error-input ()
  "Test that pb-output-filter-create-test-output creates the expected output when given a input signifying build error"
  (let ((result (pb-output-filter-create-test-output (mock-pb-test-error-message))))
    (should (string= "> fcrepo-solr-testrunner: 1 test - errors=1" result))
    (should (eq 'pb-error-face (get-text-property 0 'face result)))))

(ert-deftest test-pb-output-filter-create-test-output-raises-if-build-id-is-unknown ()
  "Test that pb-output-filter-create-test-output raises an error if build id is unknown"
  (should-error (pb-output-filter-create-test-output "unknown id - foo")))

(ert-deftest test-pb-output-filter-create-build-output-creates-the-expected-build-modeline-without-optional-param ()
  "Test that pb-output-filter-create-build-output creates the expected propertized string without optional modeline"
  (let ((result (pb-output-filter-create-build-output (mock-pb-build-start-build-message))))
    (should (string= ".b." result))
    (should (eq nil (get-text-property 1 'face result)))))

(ert-deftest test-pb-output-filter-create-build-output-creates-the-expected-success-result-modeline ()
  "Test that the expected propertized modeline is created from success build input"
  (let ((result (pb-output-filter-create-build-output (mock-pb-build-success-message) " pybuilder")))
    (should (string= "  .--.fcrepo-solr-testrunner: 7 / 85.78%  " result))
    (should (eq 'pb-modeline-success-face (get-text-property 1 'face result)))))

(ert-deftest test-pb-output-filter-create-build-output-creates-the-expected-failure-result-modeline ()
  "Test that the expected propertized modeline is created from failure build input"
  (let ((result (pb-output-filter-create-build-output (mock-pb-build-failure-message) " pybuilder")))
    (should (string= "  .--.fcrepo-solr-testrunner: failures=1  " result))
    (should (eq 'pb-modeline-failure-face (get-text-property 1 'face result)))))
 
(ert-deftest test-pb-output-filter-create-build-output-creates-the-expected-error-result-modeline ()
  "Test that the expected propertized modeline is created from error build input"
  (let ((result (pb-output-filter-create-build-output (mock-pb-build-error-message) " pybuilder")))
    (should (string= "  .--.fcrepo-solr-testrunner: errors=1  " result))
    (should (eq 'pb-modeline-error-face (get-text-property 1 'face result)))))
 
(ert-deftest test-pb-output-filter-create-build-output-raises-if-build-id-is-unknown ()
  "Test that pb-output-filter-create-build-output raises an error if build id is unknown"
  (should-error (pb-output-filter-create-build-output "unknown id - foo")))

(provide 'test-pb-output-filters)

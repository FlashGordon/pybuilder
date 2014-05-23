(require  'ert)
(require 'pb-fixtures)
(require 'with-fixtures)
(require 'test-pb-init)
(require 'pb-coverage-report)


(defun mock-coverage-lines ()
  '((("line" ("hits" . "1") ("number" . "4"))) (("line" ("hits" . "1") ("number" . "5"))) (("line" ("hits" . "1") ("number" . "6"))) (("line" ("hits" . "1") ("number" . "7"))) (("line" ("hits" . "1") ("number" . "8"))) (("line" ("hits" . "1") ("number" . "9"))) (("line" ("hits" . "1") ("number" . "10"))) (("line" ("hits" . "1") ("number" . "11"))) (("line" ("hits" . "1") ("number" . "13"))) (("line" ("hits" . "1") ("number" . "14"))) (("line" ("hits" . "1") ("number" . "16"))) (("line" ("hits" . "1") ("number" . "17"))) (("line" ("hits" . "1") ("number" . "19"))) (("line" ("hits" . "1") ("number" . "22"))) (("line" ("hits" . "1") ("number" . "25"))) (("line" ("hits" . "1") ("number" . "27"))) (("line" ("hits" . "1") ("number" . "32"))) (("line" ("hits" . "1") ("number" . "33"))) (("line" ("hits" . "1") ("number" . "36"))) (("line" ("hits" . "1") ("number" . "37"))) (("line" ("hits" . "1") ("number" . "40"))) (("line" ("hits" . "0") ("number" . "41"))) (("line" ("hits" . "1") ("number" . "44"))) (("line" ("hits" . "1") ("number" . "45"))) (("line" ("hits" . "1") ("number" . "48"))) (("line" ("hits" . "1") ("number" . "50"))) (("line" ("hits" . "1") ("number" . "52"))) (("line" ("hits" . "1") ("number" . "54"))) (("line" ("hits" . "1") ("number" . "56"))) (("line" ("hits" . "1") ("number" . "57"))) (("line" ("hits" . "1") ("number" . "59"))) (("line" ("hits" . "1") ("number" . "61"))) (("line" ("hits" . "1") ("number" . "64"))) (("line" ("hits" . "1") ("number" . "65"))) (("line" ("hits" . "1") ("number" . "66"))) (("line" ("hits" . "1") ("number" . "68"))) (("line" ("hits" . "1") ("number" . "70"))) (("line" ("hits" . "1") ("number" . "72"))) (("line" ("hits" . "1") ("number" . "76"))) (("line" ("hits" . "1") ("number" . "77"))) (("line" ("hits" . "1") ("number" . "80"))) (("line" ("hits" . "1") ("number" . "81"))) (("line" ("hits" . "1") ("number" . "82"))) (("line" ("hits" . "1") ("number" . "83"))) (("line" ("hits" . "1") ("number" . "85"))) (("line" ("hits" . "1") ("number" . "89"))) (("line" ("hits" . "1") ("number" . "90"))) (("line" ("hits" . "1") ("number" . "93"))) (("line" ("hits" . "1") ("number" . "94"))) (("line" ("hits" . "1") ("number" . "95"))) (("line" ("hits" . "1") ("number" . "96"))) (("line" ("hits" . "1") ("number" . "97"))) (("line" ("hits" . "1") ("number" . "99"))) (("line" ("hits" . "1") ("number" . "102"))) (("line" ("hits" . "1") ("number" . "104"))) (("line" ("hits" . "1") ("number" . "105"))) (("line" ("hits" . "1") ("number" . "107"))) (("line" ("hits" . "1") ("number" . "109"))) (("line" ("hits" . "1") ("number" . "111"))) (("line" ("hits" . "1") ("number" . "112"))) (("line" ("hits" . "1") ("number" . "114"))) (("line" ("hits" . "1") ("number" . "115"))) (("line" ("hits" . "1") ("number" . "117"))) (("line" ("hits" . "1") ("number" . "120"))) (("line" ("hits" . "1") ("number" . "121"))) (("line" ("hits" . "1") ("number" . "123"))) (("line" ("hits" . "1") ("number" . "124"))) (("line" ("hits" . "1") ("number" . "126"))) (("line" ("hits" . "1") ("number" . "127"))) (("line" ("hits" . "1") ("number" . "129"))) (("line" ("hits" . "1") ("number" . "131"))) (("line" ("hits" . "1") ("number" . "133"))) (("line" ("hits" . "1") ("number" . "136"))) (("line" ("hits" . "1") ("number" . "137"))) (("line" ("hits" . "1") ("number" . "139"))) (("line" ("hits" . "1") ("number" . "140"))) (("line" ("hits" . "1") ("number" . "142"))) (("line" ("hits" . "1") ("number" . "143"))) (("line" ("hits" . "1") ("number" . "145"))) (("line" ("hits" . "1") ("number" . "146"))) (("line" ("hits" . "1") ("number" . "147"))) (("line" ("hits" . "1") ("number" . "148"))) (("line" ("hits" . "1") ("number" . "150"))) (("line" ("hits" . "1") ("number" . "152"))) (("line" ("hits" . "1") ("number" . "154"))) (("line" ("hits" . "1") ("number" . "156"))) (("line" ("hits" . "1") ("number" . "160"))) (("line" ("hits" . "1") ("number" . "161"))) (("line" ("hits" . "1") ("number" . "163"))) (("line" ("hits" . "1") ("number" . "164"))) (("line" ("hits" . "1") ("number" . "166"))) (("line" ("hits" . "1") ("number" . "170"))) (("line" ("hits" . "1") ("number" . "171"))) (("line" ("hits" . "1") ("number" . "173"))) (("line" ("hits" . "1") ("number" . "174"))) (("line" ("hits" . "1") ("number" . "175"))) (("line" ("hits" . "1") ("number" . "177"))) (("line" ("hits" . "0") ("number" . "178")))))


(defun mock-coverage-lines-expected-result ()
  '((178 178 0) (177 177 1) (173 175 1) (170 171 1) (166 166 1) (163 164 1) (160 161 1) (156 156 1) (154 154 1) (152 152 1) (150 150 1) (145 148 1) (142 143 1) (139 140 1) (136 137 1) (133 133 1) (131 131 1) (129 129 1) (126 127 1) (123 124 1) (120 121 1) (117 117 1) (114 115 1) (111 112 1) (109 109 1) (107 107 1) (104 105 1) (102 102 1) (99 99 1) (93 97 1) (89 90 1) (85 85 1) (80 83 1) (76 77 1) (72 72 1) (70 70 1) (68 68 1) (64 66 1) (61 61 1) (59 59 1) (56 57 1) (54 54 1) (52 52 1) (50 50 1) (48 48 1) (44 45 1) (41 41 0) (40 40 1) (36 37 1) (32 33 1) (27 27 1) (25 25 1) (22 22 1) (19 19 1) (16 17 1) (13 14 1) (4 11 1)))

(defun mock-coverage-expected-result ()
'(("src/example_project/tests/test_example.py" ((1 1 1) (3 5 1) (8 8 1) (13 13 1) (15 16 1))) ("src/example_project/tests/__init__.py" ((0 0 0))) ("src/example_project/example_two.py" ((1 1 1) (2 3 0) (6 6 1) (8 8 1) (9 9 0) (11 11 1) (12 12 0))) ("src/example_project/example.py" ((1 1 1) (2 3 0) (6 6 1) (8 9 1) (11 12 1))) ("src/example_project/__init__.py" ((0 0 0)))))


(ert-deftest test-pb-read-xml-returns-expected-xml ()
  "Test that xml is read and returned as expected"
  (let ((xml (pb-read-xml (pb-fix-example-xml))))

    (should (string= (car xml)  "test"))
    (should (string= (cadr xml)  "test-string"))))


(ert-deftest test-pb-parse-coverage-lines-xml-returns-expected-result ()
  "Test that pb-parse-coverage-lines-xml returns expected result"
  (let ((coverage-result (pb-parse-coverage-lines-xml (mock-coverage-lines))))

    (should (equal (mock-coverage-lines-expected-result)
                   coverage-result))))

(ert-deftest test-pb-parse-coverage-lines-xml-returns-expected-result-when-input-is-nil ()
  "Test that pb-parse-coverage-lines-xml returns expected result when input is nil"
  (let ((coverage-result (pb-parse-coverage-lines-xml nil)))

    (should (equal '((0 0 0))
                   coverage-result))))


(ert-deftest test-pb-parse-coverage-xml-returns-expected-result ()
  "Test that pb-parse-coverage-xml returns the expected result"
  (let ((coverage-result (pb-parse-coverage-xml (pb-read-xml (pb-fix-example-coverage)))))

    (should (equal (mock-coverage-expected-result)
                   coverage-result))))


(ert-deftest test-pb-parse-coverage-xml-raises-if-xml-is-nil ()
  "Test that pb-parse-coverage-xml raises if xml is nil"
  (should-error (pb-parse-coverage-xml nil)))

(ert-deftest test-pb-example-two-is-filtered-as-expected ()
  "Test that example_two is filtered from result"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))

    (let ((coverage (pb-parse-coverage-xml (pb-read-xml (pb-fix-example-coverage)))))
      (save-excursion
        (unwind-protect
            (progn
              (find-file (cdr (assoc 'src-file files)))

              (should (equal'(("src/example_project/example.py" ((1 1 1) (2 3 0) (6 6 1) (8 9 1) (11 12 1))))
                            (pb-filter-coverage-tests coverage (cdr (assoc 'project-dir files))))))

          (progn (sleep-for 0 200)
                 (kill-buffer (file-name-nondirectory (cdr (assoc 'src-file files))))))))))

(ert-deftest test-line-numbers-are-transformed-to-positions-as-expected ()
  "Test that pb-transform-line-numbers-to-positions transforms line-numbers into expected positions"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
       (files (pb-fix-python-project temp-dir)))

    (let ((coverage (pb-parse-coverage-xml (pb-read-xml (pb-fix-example-coverage)))))
      (save-excursion
        (unwind-protect
            (progn
              (find-file (cdr (assoc 'src-file files)))
              (find-file (cdr (assoc 'setup-file files)))

              (should (equal
                       '(("src/example_project/example.py"
                          ((1 19 1) (20 55 0) (58 82 1) (84 134 1) (136 186 1))))

                       (pb-transform-line-numbers-to-positions
                        (pb-filter-coverage-tests coverage (cdr (assoc 'project-dir files)))))))

          (progn (sleep-for 0 200)
                 (kill-buffer (file-name-nondirectory (cdr (assoc 'src-file files))))
                 (kill-buffer (file-name-nondirectory (cdr (assoc 'setup-file files))))))))))

(ert-deftest test-block-coverage-status-func-changes-third-parameter-as-expected ()
  "Test that block-coverage-status-func changes the third parameter as expected"
  (with-fixtures* ((temp-dir (pb-fix-temp-dir))
                   (files (pb-fix-python-project temp-dir)))

    (let ((coverage (pb-parse-coverage-xml (pb-read-xml (pb-fix-example-coverage)))))
      (save-excursion
        (unwind-protect
            (progn
              (find-file (cdr (assoc 'src-file files)))
              (find-file (cdr (assoc 'setup-file files)))

              (should (equal
                       '(("src/example_project/example.py"
                          ((1 19 . 11) (20 55 . 10) (58 82 . 11) (84 134 . 11) (136 186 . 11))))

                       (pb-transform-line-numbers-to-positions
                        (pb-filter-coverage-tests coverage (cdr (assoc 'project-dir files)))
                        '(lambda (x) (+ x 10))))))

          (progn (sleep-for 0 200)
                 (kill-buffer (file-name-nondirectory (cdr (assoc 'src-file files))))
                 (kill-buffer (file-name-nondirectory (cdr (assoc 'setup-file files))))))))))

(provide 'test-pb-coverage-report)
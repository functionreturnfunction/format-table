;; -*- lexical-binding: t -*-

(require 'ert)
(require 'format-table)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;HELPERS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst test-data-directory (file-name-directory (or load-file-name buffer-file-name)))

(defun load-test-data-file (file)
  (with-temp-buffer
    (insert-file-contents
     (concat test-data-directory
             (file-name-as-directory "test-data")
             file))
    (buffer-string)))

(defun compare-result-lines (expected-lines actual-lines)
  (if (not expected-lines) nil
    (progn
      (should (string-equal (car expected-lines) (car actual-lines)))
      (compare-result-lines (cdr expected-lines) (cdr actual-lines)))))

(defun compare-results (expected actual)
  (let ((expected-lines (split-string expected "[
]+"))
        (actual-lines (split-string actual "[
]+")))
    (compare-result-lines expected-lines actual-lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TESTS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;format-table-pad-right;;;;;;;;;;;;;;

(ert-deftest format-table-pad-right-should-pad-value-to-length-chars-to-the-right-using-spaces ()
  (let ((expected "Foo   ")
        (actual (format-table-pad-right "Foo" 6)))
    (should (string-equal expected actual))))

;;;;;;;;;;;;;format-table-pad-center;;;;;;;;;;;;;;

(ert-deftest format-table-pad-center-should-surround-value-in-spaces-to-make-it-lenght-chars-long ()
  (let ((expected "  Foo  ")
        (actual (format-table-pad-center "Foo" 7)))
    (should (string-equal expected actual))))

(ert-deftest format-table-pad-center-should-favor-left-when-alignment-is-not-perfect ()
  (let ((expected "  Foo   ")
        (actual (format-table-pad-center "Foo" 8)))
    (should (string-equal expected actual))))

;;;;;;;;;format-table-get-max-col-widths;;;;;;;;;;

(ert-deftest format-table-get-max-col-widths-should-get-max-width-of-each-col-in-table ()
  (let ((expected '(5 2 3))
        (actual (format-table-get-max-col-widths
                 '(("123"   "1"  "12")
                   ("1234"  "12" "1")
                   ("12345" "1"  "123")))))
    (should (equal expected actual))))

;;;;;;;;;;;format-table-get-col-widths;;;;;;;;;;;;

(ert-deftest format-table-get-col-widths-should-get-col-widths-from-dash-line ()
  (let ((expected '(12 14 23 23 23 6 15 25 25 15 11 13 11 15 15 12 12 22 14 21 21 21 21 128 36 23))
        (actual (format-table-get-col-widths
                 (car (cdr (split-string (load-test-data-file "big-ms-input.txt") "[
]"))) (alist-get 'ms format-table-format-alist))))
    (should (equal expected actual))))

(ert-deftest format-table-get-col-widths-should-get-col-widths-for-ms ()
  (let ((expected '(13 12 21 10))
        (actual (format-table-get-col-widths
                 (car (cdr (split-string (load-test-data-file "small-ms-output.txt") "[
]"))) (alist-get 'ms format-table-format-alist))))
    (should (equal expected actual))))

(ert-deftest format-table-get-col-widths-should-get-col-widths-for-org ()
  (let ((expected '(13 12 21 10))
        (actual (format-table-get-col-widths
                 (car (cdr (split-string (load-test-data-file "org-output.txt") "[
]"))) (alist-get 'org format-table-format-alist))))
    (should (equal expected actual))))

(ert-deftest format-table-get-col-widths-should-get-col-widths-for-mysql ()
  (let ((expected '(13 12 21 10))
        (actual (format-table-get-col-widths
                 (car (cdr (cdr (split-string (load-test-data-file "mysql-output.txt") "[
]")))) (alist-get 'mysql format-table-format-alist))))
    (should (equal expected actual))))

(ert-deftest format-table-get-col-widths-should-get-col-widths-for-postgresql ()
  (let ((expected '(13 12 21 10))
        (actual (format-table-get-col-widths
                 (car (cdr (split-string (load-test-data-file "postgresql-output.txt") "[
]"))) (alist-get 'postgresql format-table-format-alist))))
    (should (equal expected actual))))

;;;;;;;;;;;;;format-table-render-row;;;;;;;;;;;;;;

(ert-deftest format-table-render-row-should-render-row-for-ms ()
  (let ((expected "foo bar
")
        (actual (format-table-render-row '("foo" "bar") '(3 3) (alist-get 'ms format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-row-should-render-row-for-org ()
  (let ((expected "| foo | bar |
")
        (actual (format-table-render-row '("foo" "bar") '(3 3) (alist-get 'org format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-row-should-render-row-for-mysql ()
  (let ((expected "| foo | bar |
")
        (actual (format-table-render-row '("foo" "bar") '(3 3) (alist-get 'mysql format-table-format-alist))))
    (should (string-equal expected actual))))

;;;;;;;;format-table-render-separator-row;;;;;;;;;

(ert-deftest format-table-render-separator-row-should-render-separator-row-for-ms ()
  (let ((expected "--- ---
")
        (actual (format-table-render-separator-row '(3 3) (alist-get 'ms format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-separator-row-should-render-separator-row-for-org ()
  (let ((expected "|-----+-----|
")
        (actual (format-table-render-separator-row '(3 3) (alist-get 'org format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-separator-row-should-render-separator-row-for-mysql ()
  (let ((expected "+-----+-----+
")
        (actual (format-table-render-separator-row '(3 3) (alist-get 'mysql format-table-format-alist))))
    (should (string-equal expected actual))))

;;;;;;;;;;;;;;format-table-split-row;;;;;;;;;;;;;;

(ert-deftest format-table-split-row-should-split-row-based-on-col-widths ()
  (let ((expected '("master" "dbo" "spt_fallback_db" "BASE TABLE"))
        (actual (format-table-split-row
                 "master        dbo          spt_fallback_db       BASE TABLE"
                 '(13 12 21 10)
                 (alist-get 'ms format-table-format-alist))))
    (should (equal expected actual))))

;;;;;;;;;;format-table-render-row-count;;;;;;;;;;;

(ert-deftest format-table-render-row-count-should-render-for-org ()
  (let ((expected "")
        (actual (format-table-render-row-count 5 (alist-get 'org format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-row-count-should-render-for-mysql ()
  (let ((expected "5 rows in set (0.00 sec)")
        (actual (format-table-render-row-count 5 (alist-get 'mysql format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-row-count-should-render-without-s-for-mysql-when-count-only-one ()
  (let ((expected "1 row in set (0.00 sec)")
        (actual (format-table-render-row-count 1 (alist-get 'mysql format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-row-count-should-render-for-postgresql ()
  (let ((expected "(5 rows)")
        (actual (format-table-render-row-count 5 (alist-get 'postgresql format-table-format-alist))))
    (should (string-equal expected actual))))

(ert-deftest format-table-render-row-count-should-render-for-ms ()
  (let ((expected "(5 rows affected)")
        (actual (format-table-render-row-count 5 (alist-get 'ms format-table-format-alist))))
    (should (string-equal expected actual))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;format-table;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;from ms;;;;;;;;;;;;;;;;;;;;;;

(ert-deftest format-table-should-format-ms-table-as-ms ()
  (let ((expected (load-test-data-file "small-ms-output.txt"))
        (actual (format-table (load-test-data-file "small-ms-input.txt") 'ms 'ms)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-ms-table-as-org ()
  (let ((expected (load-test-data-file "org-output.txt"))
        (actual (format-table (load-test-data-file "small-ms-output.txt") 'ms 'org)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-ms-table-as-mysql ()
  (let ((expected (load-test-data-file "mysql-output.txt"))
        (actual (format-table (load-test-data-file "small-ms-output.txt") 'ms 'mysql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-ms-table-as-postgresql ()
  (let ((expected (load-test-data-file "postgresql-output.txt"))
        (actual (format-table (load-test-data-file "small-ms-output.txt") 'ms 'postgresql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-ms-table-as-json ()
  (let ((expected (load-test-data-file "json-output.json"))
        (actual (format-table (load-test-data-file "small-ms-output.txt") 'ms 'json)))
    (compare-results expected actual)))

(ert-deftest format-table-should-not-choke-on-ms-string-with-no-table ()
  (let ((expected "(5 rows affected)"))
    (should (string-equal expected (format-table expected 'ms 'ms)))))

(ert-deftest format-table-should-format-larger-ms-table-as-ms ()
  (let ((expected (load-test-data-file "big-ms-output.txt"))
        (actual (format-table (load-test-data-file "big-ms-output.txt") 'ms 'ms)))
    (compare-results expected actual)))

;;;;;;;;;;;;;;;;;;;;;from org;;;;;;;;;;;;;;;;;;;;;

(ert-deftest format-table-should-format-org-table-as-ms ()
  (let ((expected (load-test-data-file "small-ms-output.txt"))
        (actual (format-table (load-test-data-file "org-output.txt") 'org 'ms)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-org-table-as-org ()
  (let ((expected (load-test-data-file "org-output.txt"))
        (actual (format-table (load-test-data-file "org-output.txt") 'org 'org)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-org-table-as-mysql ()
  (let ((expected (load-test-data-file "mysql-output.txt"))
        (actual (format-table (load-test-data-file "org-output.txt") 'org 'mysql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-org-table-as-postgresql ()
  (let ((expected (load-test-data-file "postgresql-output.txt"))
        (actual (format-table (load-test-data-file "org-output.txt") 'org 'postgresql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-org-table-as-json ()
  (let ((expected (load-test-data-file "json-output.json"))
        (actual (format-table (load-test-data-file "org-output.txt") 'org 'json)))
    (compare-results expected actual)))

;;;;;;;;;;;;;;;;;from postgresql;;;;;;;;;;;;;;;;;;

(ert-deftest format-table-should-format-postgresql-table-as-ms ()
  (let ((expected (load-test-data-file "small-ms-output.txt"))
        (actual (format-table (load-test-data-file "postgresql-output.txt") 'postgresql 'ms)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-postgresql-table-as-org ()
  (let ((expected (load-test-data-file "org-output.txt"))
        (actual (format-table (load-test-data-file "postgresql-output.txt") 'postgresql 'org)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-postgresql-table-as-mysql ()
  (let ((expected (load-test-data-file "mysql-output.txt"))
        (actual (format-table (load-test-data-file "postgresql-output.txt") 'postgresql 'mysql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-postgresql-table-as-postgresql ()
  (let ((expected (load-test-data-file "postgresql-output.txt"))
        (actual (format-table (load-test-data-file "postgresql-output.txt") 'postgresql 'postgresql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-postgresql-table-as-json ()
  (let ((expected (load-test-data-file "json-output.json"))
        (actual (format-table (load-test-data-file "postgresql-output.txt") 'postgresql 'json)))
    (compare-results expected actual)))

(ert-deftest format-table-should-not-choke-on-postgresql-string-with-no-table ()
  (let ((expected "(5 rows)"))
    (should (string-equal expected (format-table expected 'postgresql 'postgresql)))))

;;;;;;;;;;;;;;;;;;;;from mysql;;;;;;;;;;;;;;;;;;;;

(ert-deftest format-table-should-format-mysql-table-as-ms ()
  (let ((expected (load-test-data-file "small-ms-output.txt"))
        (actual (format-table (load-test-data-file "mysql-output.txt") 'mysql 'ms)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-mysql-table-as-org ()
  (let ((expected (load-test-data-file "org-output.txt"))
        (actual (format-table (load-test-data-file "mysql-output.txt") 'mysql 'org)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-mysql-table-as-postgresql ()
  (let ((expected (load-test-data-file "postgresql-output.txt"))
        (actual (format-table (load-test-data-file "mysql-output.txt") 'mysql 'postgresql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-mysql-table-as-mysql ()
  (let ((expected (load-test-data-file "mysql-output.txt"))
        (actual (format-table (load-test-data-file "mysql-output.txt") 'mysql 'mysql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-mysql-table-as-json ()
  (let ((expected (load-test-data-file "json-output.json"))
        (actual (format-table (load-test-data-file "mysql-output.txt") 'mysql 'json)))
    (compare-results expected actual)))

;;;;;;;;;;;;;;;;;;;;from json;;;;;;;;;;;;;;;;;;;;;

(ert-deftest format-table-should-format-json-table-as-ms ()
  (let ((expected (load-test-data-file "small-ms-output.txt"))
        (actual (format-table (load-test-data-file "json-output.json") 'json 'ms)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-json-table-as-org ()
  (let ((expected (load-test-data-file "org-output.txt"))
        (actual (format-table (load-test-data-file "json-output.json") 'json 'org)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-json-table-as-postgresql ()
  (let ((expected (load-test-data-file "postgresql-output.txt"))
        (actual (format-table (load-test-data-file "json-output.json") 'json 'postgresql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-json-table-as-mysql ()
  (let ((expected (load-test-data-file "mysql-output.txt"))
        (actual (format-table (load-test-data-file "json-output.json") 'json 'mysql)))
    (compare-results expected actual)))

(ert-deftest format-table-should-format-json-table-as-json ()
  (let ((expected (load-test-data-file "json-output.json"))
        (actual (format-table (load-test-data-file "json-output.json") 'json 'json)))
    (compare-results expected actual)))

(ert-deftest format-table-should-not-choke-on-mysql-string-with-no-table ()
  (let ((expected "5 rows in set (0.00 sec)"))
    (should (string-equal expected (format-table expected 'mysql 'mysql)))))

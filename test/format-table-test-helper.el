;;; format-table-test-helper.el --- Helpers for format-table-test.el -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*-tests.el")))

(require 'format-table)

(provide 'format-table-test-helper)
;;; format-table-test-helper.el ends here

;;; format-table.el --- Parse and reformat tabular data.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 Jason Duncan, all rights reserved

;; Author:  Jason Duncan <jasond496@msn.com>
;; Version: 0.0.1
;; Keywords: data
;; URL: https://github.com/functionreturnfurnction/format-table
;; Package-Requires: ((emacs "25") (dash "2.14.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Parse and reformat tabular data.

;;; Code:

(require 'dash)
(require 'json)

(defvar format-table-format-alist
  '((ms
     :begin-row               ""
     :end-row                 ""
     :col-separator           " "
     :separator-col-separator " "
     :separator-begin-row     ""
     :separator-end-row       ""
     :row-count-format        "(%s rows affected)")

    (org
     :begin-row               "| "
     :end-row                 " |"
     :col-separator           " | "
     :separator-col-separator "-+-"
     :separator-begin-row     "|-"
     :separator-end-row       "-|")

    (mysql
     :top-border-fn           format-table-render-separator-row
     :bottom-border-fn        format-table-render-separator-row
     :begin-row               "| "
     :end-row                 " |"
     :col-separator           " | "
     :separator-col-separator "-+-"
     :separator-begin-row     "+-"
     :separator-end-row       "-+"
     :row-count-format        "%s rows? in set ([[:digit:]]+.[[:digit:]]+ sec)")

    (postgresql
     :header-pad-fn           format-table-pad-center
     :begin-row               " "
     :end-row                 " "
     :col-separator           " | "
     :separator-col-separator "-+-"
     :separator-begin-row     "-"
     :separator-end-row       "-"
     :row-count-format        "(%s rows)")

    (json . json)))

(defun format-table-pad-right (value length)
  "Pad the string VALUE to the length specified by LENGTH using spaces to the right."
  (format (concat "%-" (number-to-string length) "s") value))

(defun format-table-pad-center (value length)
  "Pad the string VALUE to the length specified by LENGTH by surrounding with spaces."
  (let* ((value-length (length value))
         (left-length (/ (- length value-length) 2))
         (right-length (+ left-length
                          (if (or
                               (and (= 1 (% value-length 2))
                                    (= 0 (% length 2)))
                               (and (= 0 (% value-length 2))
                                    (= 1 (% length 2))))
                              1 0))))
    (concat
     (make-string left-length ? )
     value
     (make-string right-length ? ))))

(defun format-table-remove-noise (lines input-mode)
  "Given the set of table LINES and some extra information in INPUT-MODE, filter out any empty lines or lines which otherwise do not belong to the table of values."
  (let* ((row-count-format (plist-get input-mode :row-count-format))
         (regexp (if (not row-count-format) nil (format row-count-format "[[:digit:]]+")))
         ret)
    (dolist (cur-line (reverse lines) ret)
      (if (not (or (string-equal "" cur-line)
                   (if (not regexp) nil (string-match regexp cur-line))))
          (push cur-line ret)))
    (if (not ret) nil
      (setq ret (if (plist-get input-mode :top-border-fn) (-slice ret 1) ret))
      (if (plist-get input-mode :top-border-fn)
          (-slice ret 0 (1- (length ret))) ret))))

(defun format-table-trim-row (row begin-row end-row)
  "Given the string ROW trim the string BEGIN-ROW from the beginning and END-ROW from the end."
  (replace-regexp-in-string
   (concat "^" (regexp-quote begin-row))
   ""
   (replace-regexp-in-string
    (concat (regexp-quote end-row) "$")
    ""
    row)))

(defun format-table-get-col-widths (dashes input-mode)
  "Using the line of all DASHES from the table and the INPUT-MODE, determine the widths of the columns and return them in a list."
  (let* ((separator-begin-row (plist-get input-mode :separator-begin-row))
         (separator-end-row (plist-get input-mode :separator-end-row))
         (separator-col-separator (plist-get input-mode :separator-col-separator))
         (dashes (format-table-trim-row dashes separator-begin-row separator-end-row)))
    (-map 'length (split-string dashes
                                (regexp-quote separator-col-separator)))))

(defun format-table-split-row (row col-widths input-mode)
  "Split the given string ROW based on the fixed positions listed in COL-WIDTHS and any additional information in INPUT-MODE."
  (let* ((begin-row (plist-get input-mode :begin-row))
         (end-row (plist-get input-mode :end-row))
         (col-separator (plist-get input-mode :col-separator))
         (row (format-table-trim-row row begin-row end-row))
         split)
    (reverse
     (dolist (cur-width col-widths split)
       (let* ((row-padded (if (> cur-width (length row))
                              (format-table-pad-right row cur-width)
                            row)))
         (push (string-trim (substring row-padded 0 cur-width)) split)
         ;; trim off the spaces used to separate columns, but only if it's there (might not be on last row)
         (setq row (substring row-padded
                              (if (> (length row-padded) cur-width)
                                  (+ cur-width (length col-separator))
                                cur-width))))))))

(defun format-table-assemble-table (header body)
  "Given the HEADER list of column names and nested list of values BODY, return an alist with both and some extra meta information about same."
  (let ((max-col-widths (format-table-get-max-col-widths (cons header body))))
    (list :header header
          :body body
          :max-col-widths max-col-widths
          :row-count (length body))))

(defun format-table-parse-table (lines col-widths input-mode)
  "Given the list of table LINES, set of COL-WIDTHS, and INPUT-MODE, build a table of values as a plist."
  (let* ((header (format-table-split-row (car lines) col-widths input-mode))
         (body (--map (format-table-split-row it col-widths input-mode) (-slice lines 2))))
    (format-table-assemble-table header body)))

(defun format-table-get-max-col-widths (table)
  "Given the nested list TABLE of values, determine the length of the longest value in each column and return each in a list."
  (let ((last (make-list (length (car table)) 0)))
    (dolist (cur-row table last)
      (setq last
            (-zip-with 'max (-map 'length cur-row) last)))))

(defun format-table-render-row (row max-col-widths output-mode &optional pad-fn)
  "Render a table row with the proper column separators and a newline.

Arguments are the list of values ROW, the list of MAX-COL-WIDTHS, and delimiter
information in OUTPUT-MODE.  Optionally use PAD-FN to pad each column value,
otherwise values will be padded to the right with spaces."
  (let ((pad-fn (or pad-fn 'format-table-pad-right)))
    (concat
     (plist-get output-mode :begin-row)
     (string-join
      (-zip-with pad-fn row max-col-widths)
      (plist-get output-mode :col-separator))
     (plist-get output-mode :end-row)
     hard-newline)))

(defun format-table-generate-dash-string (length)
  "Generate a string of hyphens LENGTH chars long."
  (make-string length ?-))

(defun format-table-render-separator-row (max-col-widths output-mode)
  "Given the list of MAX-COL-WIDTHS and delimiter information in OUTPUT-MODE, render a row which separates the header row from the rest of the rows."
  (concat (plist-get output-mode :separator-begin-row)
          (string-join
           (-map 'format-table-generate-dash-string max-col-widths)
           (plist-get output-mode :separator-col-separator))
          (plist-get output-mode :separator-end-row)
          hard-newline))

(defun format-table-render-json (table)
  "Render the TABLE of values as a json string."
  (let ((vec []))
    (dolist (cur-row (plist-get table :body) vec)
      (let ((rec (-zip-with 'cons (plist-get table :header) cur-row)))
        (setq vec (vconcat vec (list rec)))))
    (json-encode vec)))

(defun format-table-render-table (table output-mode)
  "Given the TABLE of values and delimiter information in OUTPUT-MODE, re-render the table as a string."
  (if (equal output-mode 'json)
      (format-table-render-json table)
    (let ((top-border-fn (plist-get output-mode :top-border-fn))
          (bottom-border-fn (plist-get output-mode :bottom-border-fn))
          (header-pad-fn (plist-get output-mode :header-pad-fn))
          (max-col-widths (plist-get table :max-col-widths)))
      (concat
       (if top-border-fn (funcall top-border-fn max-col-widths output-mode))
       (format-table-render-row (plist-get table :header) max-col-widths output-mode header-pad-fn)
       (format-table-render-separator-row max-col-widths output-mode)
       (string-join
        (--map (format-table-render-row it max-col-widths output-mode) (plist-get table :body)))
       (if bottom-border-fn (funcall bottom-border-fn max-col-widths output-mode))))))

(defun format-table-render-row-count (count output-mode)
  "Render the given row count COUNT for the given OUTPUT-MODE."
  (let ((output-format (plist-get output-mode :row-count-format)))
    (if (not output-format) ""
      (replace-regexp-in-string
       (regexp-quote "[[:digit:]]+.[[:digit:]]+") "0.00"
       (replace-regexp-in-string
        (regexp-quote (if (= 1 count) "s?" "?")) ""
        (format output-format count))))))

(defun format-table-get-format (mode)
  "Return the MODE from `format-table-format-alist', erroring if not found."
  (or (alist-get mode format-table-format-alist)
      (error (format "Format mode %s not recognized." mode))))

(defun format-table-parse-json-gather-column-values (obj)
  "Gather cdrs from alist OBJ."
  (-map 'cdr obj))

(defun format-table-parse-json-gather-column-names (obj)
  "Gather cars from alist OBJ."
  (-map 'car obj))

(defun format-table-parse-json (str)
  "Parse the json string STR to a table of values as a plist."
  (let* ((json-key-type 'string)
         (vec (append (json-read-from-string str) nil))
         (header (format-table-parse-json-gather-column-names (car vec)))
         (body (-map 'format-table-parse-json-gather-column-values vec)))
    (format-table-assemble-table header body)))

(defun format-table-cleanup-and-parse (str input-mode)
  "Parse the given string STR using delimiater information in INPUT-MODE to a table of values as a plist."
  (if (equal input-mode 'json)
      (format-table-parse-json str)
    (let* ((lines (split-string str "[
]+"))
           (lines (format-table-remove-noise lines input-mode)))
      (if (not lines) nil
        (let* ((col-widths (format-table-get-col-widths (car (cdr lines)) input-mode)))
          (format-table-parse-table lines col-widths input-mode))))))

(defun format-table (str input-mode output-mode)
  "Process the given string STR containing a table in a format specified by INPUT-MODE, gather and reformat the table contained within to the format specified by OUTPUT-MODE."
  (let* ((input-mode (format-table-get-format input-mode))
         (output-mode (format-table-get-format output-mode))
         (table (format-table-cleanup-and-parse str input-mode)))
    (if (not table)
        str
      (concat
       (format-table-render-table table output-mode)
       (format-table-render-row-count (plist-get table :row-count) output-mode)))))

(provide 'format-table)
;;; format-table.el ends here

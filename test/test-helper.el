;; -*- lexical-binding: t; -*-

(when (require 'undercover nil t)
  (undercover "*.el"
              (:exclude "*-tests.el")))

(require 'format-table)

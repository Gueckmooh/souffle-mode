;;; souffle-mode.el --- Major mode for Souffle datalog files. -*- lexical-binding: t -*-

;; Copyright (C) 2017 Erik Post

;; Author   : Erik Post <erik@shinsetsu.nl>
;; Homepage : https://github.com/epost/souffle-mode
;; Version  : 0.1.0
;; Keywords : languages

;;; Commentary:

;; Emacs integration for Souffle datalog files

;;; Code:



(defconst souffle-mode-syntax-table
    (let ((table (make-syntax-table)))
        (modify-syntax-entry ?/ "< 1" table)
        (modify-syntax-entry ?/ "< 2" table)
        (modify-syntax-entry ?\n "> " table)
        table)
    "Souffle mode syntax table.")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Souffle font locks start here ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst souffle-dot-keywords
    (list "type" "decl" "comp" "init" "input" "output" "number_type" "symbol_type" "override" "printsize")
    "Souffle keywords that start with a dot.")

(defconst souffle-string-functions
    (list "cat" "contains" "match" "ord" "strlen" "substr" "to_number" "to_string")
    "Souffle builtin string functions.")

(defconst souffle-aggregation-functions
    (list "min" "max" "sum" "count")
    "Souffle builtin aggregation functions.")

(defconst souffle-types
    (list "symbol" "number" "unsigned" "float")
    "Souffle builtin types.")

(defconst souffle-highlights
    (let* (
              ;; Generate regexp for each category.
              (souffle-dot-keywords-regexp
                  (concat
                      "\\.\\("
                      (regexp-opt souffle-dot-keywords 'symbols)
                      "\\)"))

              (souffle-string-fuctions-regexp
                  (regexp-opt souffle-string-functions 'symbols))

              (souffle-aggregation-functions-regexp
                  (regexp-opt souffle-aggregation-functions 'symbols))

              (souffle-types-regexp
                  (regexp-opt souffle-types 'symbols))
            )

        `(
             (,souffle-dot-keywords-regexp . font-lock-keyword-face)
             (,souffle-string-fuctions-regexp . font-lock-function-name-face)
             (,souffle-aggregation-functions-regexp . font-lock-function-name-face)
             (,souffle-types-regexp . font-lock-type-face)
             )
        ))



;;;;;;;;;;;;;;;;;;;;;;;
;; define major mode ;;
;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode souffle-mode prog-mode "souffle"
  "major mode for editing Souffle datalog files."
    :syntax-table souffle-mode-syntax-table
  
    (setq font-lock-defaults '(souffle-highlights))
    
    (setq-local comment-start "\/\/")
    (setq-local comment-end "")
 )


(add-to-list 'auto-mode-alist '("\\.dl\\'" . souffle-mode))

(provide 'souffle-mode)

;;; souffle-mode.el ends here

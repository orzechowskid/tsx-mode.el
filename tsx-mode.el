(require 'js)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tsi-typescript)

(defvar tsx-mode--syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?` "\"" table)
    (modify-syntax-entry ?$ "_" table)
    table))

(define-derived-mode
    tsx-mode prog-mode "TSX"
    "A batteries-included major mode for modern webapps."
    :group 'tsx-mode
    :syntax-table tsx-mode--syntax-table

    ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/84
    (unless font-lock-defaults
      (setq font-lock-defaults '(nil)))

    ;; re-use the existing TS[X] data shipped with tree-sitter
    (setq tree-sitter-hl-default-patterns (tree-sitter-langs--hl-default-patterns 'tsx))
    (tree-sitter-hl-mode)
    (tsi-typescript-mode))

(provide 'tsx-mode)

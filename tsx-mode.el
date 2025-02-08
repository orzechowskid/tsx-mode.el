;;; tsx-mode.el --- a batteries-included major mode for TSX and friends -*- lexical-binding: t -*-

;;; Version: 4.0.0

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsx-mode.el

;;; Package-Requires: ((emacs "30.0") (treesit-fold "0.1.0"))

;;; Commentary:

;;; Code:

(require 'css-mode)
(require 'eglot)
(require 'treesit)


(defcustom tsx-mode-enable-css-in-js
	t
	"Conditionally or unconditionally enable or disable tracking of CSS-in-JS
   ranges."
	:type '(choice (const :tag "Never" nil)
								 (const :tag "When point is in a range" when-in-range)
								 (const :tag "Always" t)))

(defcustom tsx-mode-enable-folding
	t
	"Enable or disable code folding for blocks, functions, etc.")

(defcustom tsx-mode-enable-lsp
	t
	"Enable or disable LSP support with eglot (and typescript-language-server).")


(defvar-local tsx-mode/current-range
	nil
	"Internal variable.  a cons cell containing the start and end buffer positions
   of the current embedded range, or `nil' if none.")

(defvar tsx-mode/css-queries
	(list
	 ;; styled-components, emotion, etc. format
	 ;; styled.foo``
	 (treesit-query-compile 'tsx
													'((member_expression ((identifier) @_id) (:match "styled" @_id)) (template_string) @ts)
													t)
	 ;; styled(Foo)``
	 (treesit-query-compile 'tsx
													'((call_expression ((identifier) @_id) (:match "styled" @_id)) (template_string) @ts)
													t)
	 ;; css``
	 (treesit-query-compile 'tsx
													'((call_expression (identifier) @_id (:match "css" @_id) (template_string) @ts))
													t)

 	 ;; styled-jsx format
 	 ;; <style jsx>{``}</style>
	 (treesit-query-compile 'tsx
 													'((jsx_element (jsx_opening_element (identifier) @_name (:match "style" @_name) (jsx_attribute (property_identifier) @_attr (:match "jsx" @_attr))) (jsx_expression (template_string) @ts)))
													t)

	 ;; Quik format
	 ;; useStyles$(``) or useStylesScoped$(``)
 	 (treesit-query-compile 'tsx
													'((call_expression (identifier) @_id (:match "^useStyles\\(Scoped\\)?\\$" @_id) (arguments (template_string) @ts)))
													t))
	"Internal variable.  Treesit queries for the TSX language, specifying known
   CSS-in-JS ranges.")

(defvar tsx-mode/css-indent-rules
	(append '(css-in-js)
					'(
						;; this rule doesn't exist (and doesn't need to) in css-mode
						((parent-is "stylesheet") parent-bol css-indent-offset)
						;; change alignment of multi-line property values
						((parent-is "declaration") parent-bol css-indent-offset))
					(cdar css--treesit-indent-rules))
	"Internal variables.  Tree-sitter indentation settings for CSS-in-JS.  Derived
   from `css--treesit-indent-rules', with a couple of additions.")

(defvar tsx-mode/css-font-lock-rules
  (list
	 :default-language 'css-in-js

   :feature 'comment
   '((comment) @font-lock-comment-face)

   :feature 'string
   '((string_value) @font-lock-string-face)

   :feature 'keyword
   '(["@media"
      "@import"
      "@charset"
      "@namespace"
      "@keyframes"] @font-lock-builtin-face
      ["and"
       "or"
       "not"
       "only"
       "selector"] @font-lock-keyword-face)

   :feature 'variable
   '((plain_value) @tsx-mode--css-font-lock-value
		 (color_value) @tsx-mode--css-font-lock-value)

   :feature 'operator
   `(["=" "~=" "^=" "|=" "*=" "$="] @font-lock-operator-face)

   :feature 'selector
   '((class_selector) @css-selector
     (child_selector) @css-selector
     (id_selector) @css-selector
     (tag_name) @css-selector
     (class_name) @css-selector)

   :feature 'property
   `((property_name) @font-lock-property-name-face)

   :feature 'function
   '((function_name) @font-lock-function-name-face)

   :feature 'constant
   '((integer_value) @font-lock-number-face
     (float_value) @font-lock-number-face
     (unit) @font-lock-constant-face
     (important) @font-lock-builtin-face)

   :feature 'query
   '((keyword_query) @font-lock-property-use-face
     (feature_name) @font-lock-property-use-face)

   :feature 'bracket
   '((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face))
  "Internal variable.  Tree-sitter font-lock settings for CSS-in-JS.  Copied
   from `css-ts-mode', in where they are not available as a separate variable.")


(defun tsx-mode--css-font-lock-value (node override start end &rest _unused)
	"Internal function.  Apply font-locking to some CSS property values."
	;; n.b. this function should really be named 'tsx-mode/css-font-lock-value' to
	;; indicate that it's an internal-only function, but treesit doesn't seem to
	;; like functions with '/' characters in their names to be used as captures
	(let* ((node-text (treesit-node-text node t))
				 (node-type (treesit-node-type node))
				 (node-start (treesit-node-start node))
				 (node-end (treesit-node-end node)))
		(when (or (eq tsx-mode-enable-css-in-js t)
							(and tsx-mode/current-range
									 (eq tsx-mode-enable-css-in-js 'when-in-range)))
			(add-text-properties node-start
													 node-end
													 (cond
														((or (string= node-type "color_value")
																 (member node-text x-colors))
														 `(face (:background
																		 ,node-text ; uses text value as bgcolor
																		 :foreground
																		 ,(readable-foreground-color node-text))))
														((or (string= node-text "auto")
																 (string= node-text "inherit")
																 (string= node-text "initial")
																 (string= node-text "unset"))
														 '(face font-lock-keyword-face))
														(t '(face font-lock-variable-name-face)))))))

(defun tsx-mode/capf-css ()
	"Internal function.  completion-at-point function for CSS-in-JS embedded
   ranges."
	(or
	 ;; CSS property name completion
	 ;; the original expects properties to be preceded with a '{' or ';' which
	 ;; will not be true for most CSS properties in CSS-in-JS ranges.  we need to
	 ;; search for backticks too.  (technically what we need to search for is the
	 ;; range delimiter, but that delimiter is a backtick in all currently-
	 ;; supported cases)
	 (save-excursion
		 (let ((pos (point)))
			 (skip-chars-backward "-[:alnum:]")
			 (let ((start (point)))
				 (skip-chars-backward " \t\r\n")
         (when (memq (char-before)
										 '(?\{ ?\; ?\`))
           (list start
								 pos
								 css-property-ids)))))
		 ;; CSS property value completion
		 ;; the original uses `syntax-ppss' to restrict the lookback area, which
		 ;; seems to apply to tsx sexps instead of css-in-js ones we want
	 (save-excursion
		 (save-match-data
			 (let ((property (and (looking-back "\\([[:alnum:]-]+\\):.*"
																					(min (point)
																							 (or (car tsx-mode/current-range)
																									 most-positive-fixnum))
																					t)
														(member (match-string-no-properties 1)
																		css-property-ids))))
				 (when property
					 (let ((end (point)))
						 (save-excursion
							 (skip-chars-backward "[:graph:]")
							 (list (point)
										 end
										 (append '("inherit" "initial" "unset")
														 (css--property-values (car property))))))))))
	 ;; try the native css-mode capf
	 (css--complete-property)))

(defun tsx-mode/language-at-point-function (pos)
	"Internal function.  Calculates the treesit language at POS."
	(let ((next-range nil))
		(seq-find (lambda (el)
								(setq next-range
											(car (treesit-query-range 'tsx
																								el
																								(min pos (1- (buffer-size)))
																								(min (1+ pos) (1- (buffer-size)))))))
							tsx-mode/css-queries)
		(if next-range
				(cond ((eq tsx-mode-enable-css-in-js t)
							 'css-in-js)
							((and (eq tsx-mode-enable-css-in-js 'when-in-range)
										tsx-mode/current-range
										(> pos (car tsx-mode/current-range))
										(< pos (cdr tsx-mode/current-range)))
							 'css-in-js)
							(nil
							 'tsx))
			'tsx)))

(defun tsx-mode/get-current-range ()
	"Internal function.  Recalculates the treesit embedded range containing point,
   if any."
	(let* ((pos (point))
				 (next-range nil))
		(seq-find (lambda (el)
								(setq next-range
											(car (treesit-query-range 'tsx
																								el
																								(min pos (1- (buffer-size)))
																								(min (1+ pos) (1- (buffer-size)))))))
							tsx-mode/css-queries)
		next-range))

(defun tsx-mode/post-command-hook ()
	"Internal function.  Performs some tasks related to range tracking and font-
   locking after a change in point."
	(let ((prev-range tsx-mode/current-range))
		(setq-local tsx-mode/current-range (tsx-mode/get-current-range))
		(when prev-range
			(save-excursion
				(treesit-font-lock-fontify-region (car prev-range)
																					(cdr prev-range))))
		(when tsx-mode/current-range
			(save-excursion
				(treesit-font-lock-fontify-region (car tsx-mode/current-range)
																					(cdr tsx-mode/current-range))))))

(defun tsx-mode/capf ()
	"Internal function.  completion-at-point function for embedded treesit ranges.

   This function is suitable for use in `completion-at-point-functions'."
	(or (tsx-mode/capf-css)))

(defun tsx-mode/eglot-managed-mode-hook ()
	"Internal function.  Override some things which `eglot-ensure' does for us, to
   preserve awareness of embedded treesit regions."
	(add-to-list 'completion-at-point-functions
							 #'tsx-mode/capf))

;;;###autoload
(define-derived-mode
	tsx-mode tsx-ts-mode "TSX"
	"A batteries-included major mode for TSX and friends."
	:group 'tsx-mode
	(unless (treesit-ready-p 'css-in-js)
		(error "CSS-in-JS parser not ready"))
	(setq-local
	 treesit-primary-parser (treesit-parser-create 'tsx)
	 treesit-language-at-point-function #'tsx-mode/language-at-point-function
	 treesit-range-settings (apply #'treesit-range-rules
																 (seq-reduce (lambda (acc el)
																							 (append acc
																											 (list :host 'tsx
																														 :embed 'css-in-js
																														 :offset '(1 . -1)
																														 :local t
																														 el)))
																						 tsx-mode/css-queries
																						 '())))
	(add-hook 'post-command-hook
						#'tsx-mode/post-command-hook nil t)
	(when tsx-mode-enable-css-in-js
		(setq-local treesit-font-lock-settings (append treesit-font-lock-settings
																									 (apply 'treesit-font-lock-rules
																													tsx-mode/css-font-lock-rules)))
		(push tsx-mode/css-indent-rules
					treesit-simple-indent-rules)
		(push `(css-in-js (text "\\(?:comment\\)" 'symbols))
					treesit-thing-settings)
			(treesit-update-ranges))
	(when tsx-mode-enable-lsp
		(add-hook 'eglot-managed-mode-hook
							#'tsx-mode/eglot-managed-mode-hook nil t)
		(eglot-ensure))
	(when tsx-mode-enable-folding
		(require 'treesit-fold)
		(define-key tsx-mode-map
								(kbd "C-c t f")
								#'treesit-fold-toggle)
		(define-key tsx-mode-map
								(kbd "C-c t F")
								#'treesit-fold-open-all)
		(treesit-fold-mode t)))

;;;###autoload
(progn
	(with-eval-after-load 'eglot
		(add-to-list 'eglot-server-programs
								 '(tsx-mode "typescript-language-server" "--stdio")))
	(with-eval-after-load 'treesit-fold
		(add-to-list 'treesit-fold-range-alist
								 `(tsx-mode . ,(alist-get 'tsx-ts-mode
																					treesit-fold-range-alist)))))

(provide 'tsx-mode)
;; tsx-mode.el ends here

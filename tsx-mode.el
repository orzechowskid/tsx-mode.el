;;; tsx-mode.el --- a batteries-included major mode for TSX and friends -*- lexical-binding: t -*-

;;; Version: 5.2.0

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsx-mode.el

;;; Package-Requires: ((emacs "30.0") (treesit-fold "0.1.0") (cov "0.1.0") (flymake-jsts "1.1.2") (indent-bars "0.9.2") (apheleia "4.4.1")))

;;; Commentary:

;; CSS-in-JS linting is currently marked as experimental since it relies on a
;; package not currently published on ELPA or MELPA (namely, `flymake-stylelint'.
;; if you want to enable `tsx-mode-enable-css-in-js-linting' then you'll have to
;; install that package yourself (either manually or via `use-package').

;;; Code:

(require 'css-mode)
(require 'eglot)
(require 'treesit)


(defgroup tsx-mode nil
	"Major mode for JS and friends."
	:group 'programming
	:prefix "tsx-mode-")


(defcustom tsx-mode-enable-css-in-js
	t
	"Conditionally or unconditionally enable or disable tracking of CSS-in-JS
   ranges."
	:type '(choice (const :tag "Never" nil)
								 (const :tag "When point is in a range" when-in-range)
								 (const :tag "Always" t))
	:group 'tsx-mode)

(defcustom tsx-mode-enable-js-linting
	t
	"Enable or disable lint reports for Javascript/Typescript."
	:type 'boolean
	:group 'tsx-mode)

(defcustom tsx-mode-enable-css-in-js-linting
	nil
	"Enable or disable lint reports for CSS-in-JS.  (experimental)"
	:type 'boolean
	:group 'tsx-mode)

(defcustom tsx-mode-enable-linting
	t
	"Enable or disable linting with ESLint."
	:type 'boolean
	:group 'tsx-mode)
(make-obsolete-variable 'tsx-mode-enable-linting
												"Prefer `tsx-mode-enable-js-linting' or `tsx-mode-enable-css-in-js-linting'"
												"5.0.0")

(defcustom tsx-mode-enable-folding
	t
	"Enable or disable code folding for blocks, functions, etc."
	:type 'boolean
	:group 'tsx-mode)

(defcustom tsx-mode-enable-coverage
	nil
	"Experimental.  Enable or disable code-coverage tools."
	:type 'boolean
	:group 'tsx-mode)

(defcustom tsx-mode-enable-lsp
	t
	"Enable or disable LSP support with eglot (and typescript-language-server)."
	:type 'boolean
	:group 'tsx-mode)

(defcustom tsx-mode-enable-code-coverage
	nil
	"Enable or disable code-coverage annotations."
	:type 'boolean
	:group 'tsx-mode)

(defcustom tsx-mode-enable-indent-hints
	t
	"Enable or disable indent-outline hints."
	:type 'boolean
	:group 'tsx-mode)

(defcustom tsx-mode-enable-format-on-save
  nil
  "Enable or disable formatting of the current buffer when saving to disk."
  :type 'boolean
  :group 'tsx-mode)


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
	(when tsx-mode/current-range
		(or
		 ;; CSS property name completion
		 ;; css-mode's capf expects properties to be preceded with a '{' or ';' which
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
		 (css--complete-property))))

(defun tsx-mode/language-at-point-function (pos)
	"Internal function.  Calculates the treesit language at POS."
	(if (> (buffer-size) 1)
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
				'tsx))
		'tsx))

(defun tsx-mode/get-current-range ()
	"Internal function.  Recalculates the treesit embedded range containing point,
   if any."
	(when (> (buffer-size) 1)
		(let* ((pos (point))
					 (next-range nil))
			(seq-find (lambda (el)
									(setq next-range
												(car (treesit-query-range 'tsx
																									el
																									(min pos (1- (buffer-size)))
																									(min (1+ pos) (1- (buffer-size)))))))
								tsx-mode/css-queries)
			next-range)))

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
	"Internal function.  Override or enhance some things which `eglot-ensure' does
for us."
	(when tsx-mode-enable-css-in-js
		(add-to-list 'completion-at-point-functions
								 #'tsx-mode/capf)))

(defun tsx-mode/coverage-find-clover (buffer-file-dir buffer-file-name)
	(let ((clover-file-path (concat (locate-dominating-file buffer-file-dir
																													"package.json")
																	(file-name-as-directory "coverage")
																	"clover.xml")))
		(when (file-exists-p clover-file-path)
			(cons clover-file-path 'clover))))

(defun tsx-mode/coverage-find-lcov (buffer-file-dir buffer-file-name)
	(let ((lcov-file-path (concat (locate-dominating-file buffer-file-dir
																												"package.json")
																(file-name-as-directory "coverage")
																"lcov.info")))
		(when (file-exists-p lcov-file-path)
			(cons lcov-file-path 'lcov))))

(defun tsx-mode/enable-js-linting ()
	"Internal function.  Enables JS/TS linting and configures a key command."
	(require 'flymake-jsts)
	(flymake-jsts-eslint-enable)
	(flymake-jsts-oxlint-enable)
	(flymake-jsts-biome-enable)
	(define-key tsx-mode-map
							(kbd "C-c t !")
							#'flymake-goto-next-error))

(defun tsx-mode/enable-css-linting ()
	"Internal function.  Enables CSS linting and configures a key command."
	(require 'flymake-stylelint)
	(flymake-stylelint-enable)
	(define-key tsx-mode-map
							(kbd "C-c t !")
							#'flymake-goto-next-error))

(defun tsx-mode/enable-linting ()
	"Internal function.  Convenience function to enable linting features."
	(when tsx-mode-enable-js-linting
		(tsx-mode/enable-js-linting))
	(when tsx-mode-enable-css-in-js-linting
		(tsx-mode/enable-css-linting)))

(defun tsx-mode/enable-some-linting-p ()
	"Internal function.  Returns t if any linting feature is enabled."
	(or tsx-mode-enable-js-linting
			tsx-mode-enable-css-in-js-linting))


;;;###autoload
(define-derived-mode
	tsx-mode tsx-ts-mode "TSX"
	"A batteries-included major mode for TSX and friends."
	:group 'tsx-mode
	(when (and tsx-mode-enable-css-in-js
						 (not (treesit-ready-p 'css-in-js)))
		(error "CSS-in-JS parser not ready"))
	(setq-local
	 treesit-primary-parser (treesit-parser-create 'tsx)
	 ;; tell project.el how to find non-vc projects, and to ignore contents of any
	 ;; node_modules directories
	 project-vc-ignores '("node_modules"))
	;; this is a slight abuse of this variable but it makes `project-find-file' go
	;; way faster
	(add-to-list (make-local-variable 'vc-directory-exclusion-list)
							 "node_modules")
	;; helper function to let project.el use package.json as the root of a project
	(add-to-list (make-local-variable 'project-find-functions)
							 (lambda (dir)
								 (when-let* ((package-json-dir (locate-dominating-file dir "package.json")))
									 `(transient . ,package-json-dir))))
	(when tsx-mode-enable-css-in-js
		(setq-local
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
																							 '()))
		 treesit-font-lock-settings (append treesit-font-lock-settings
																				(apply 'treesit-font-lock-rules
																							 tsx-mode/css-font-lock-rules)))
		(add-hook 'post-command-hook
							#'tsx-mode/post-command-hook
              nil t)
		(progn
			(push tsx-mode/css-indent-rules
						treesit-simple-indent-rules)
			(treesit-update-ranges)))
	(when tsx-mode-enable-indent-hints
		(indent-bars-mode +1))

	;; linting (if enabled) needs to be configured after lsp (if enabled)
	(cond
	 ((and tsx-mode-enable-lsp
				 (tsx-mode/enable-some-linting-p))
		(add-hook (make-local-variable 'eglot-managed-mode-hook)
							#'tsx-mode/eglot-managed-mode-hook
              nil t)
		(add-hook (make-local-variable 'eglot-managed-mode-hook)
							(lambda ()
								(tsx-mode/enable-linting))
                nil t)
		(eglot-ensure))
	 (tsx-mode-enable-lsp
		(add-hook (make-local-variable 'eglot-managed-mode-hook)
							#'tsx-mode/eglot-managed-mode-hook
              nil t)
		(eglot-ensure))
	 ((tsx-mode/enable-some-linting-p)
		(tsx-mode/enable-linting))
	 (nil t))

	(when tsx-mode-enable-coverage
		(require 'cov)
		(add-to-list 'cov-coverage-file-paths
								 #'tsx-mode/coverage-find-clover)
		(add-to-list 'cov-coverage-file-paths
								 #'tsx-mode/coverage-find-lcov)
		(setq-local cov-coverage-mode t)
		(cov-mode t))

	(when tsx-mode-enable-folding
		(require 'treesit-fold)
		(define-key tsx-mode-map
								(kbd "C-c t f")
								#'treesit-fold-toggle)
		(define-key tsx-mode-map
								(kbd "C-c t F")
								#'treesit-fold-open-all)
		(define-key tsx-mode-map
								(kbd "C-c t x")
								#'eglot-code-actions)
		(treesit-fold-mode t))

  (when tsx-mode-enable-format-on-save
    (require 'apheleia)
    (push '(tsx-mode . prettier)
          apheleia-mode-alist)
    (apheleia-mode +1)))

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

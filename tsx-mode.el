;;; tsx-mode.el --- a batteries-included major mode for JSX and friends -*- lexical-binding: t -*-

;;; Version: 3.0.0

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsx-mode.el

;;; Package-Requires: ((emacs "27") (tsi "1.0.0") (tree-sitter-langs "0.11.3") (lsp-mode "8.0.0") (origami "1.0") (coverlay "3.0.2") (graphql-mode "1.0.0"))

;;; Code:


(require 'js)
(require 'seq)

(require 'coverlay)
(require 'lsp)
(require 'lsp-completion)
;; origami depends on some now-deprecated cl functions and there's not much we
;; can do about that
(let ((byte-compile-warnings '((not cl-functions))))
  (require 'origami))
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tsi-typescript)


(defgroup tsx-mode nil
  "Major mode for JSX webapp files."
  :group 'programming
  :prefix "tsx-mode-")


(defcustom tsx-mode-tsx-auto-tags nil
  "When set to t, typing an open angle-bracket ('<') will also insert '/>` to
create a self-closing element tag.  Typing a close angle-bracket ('>') will, if
point is inside a self-closing tag, turn that tag into separate opening and
closing tags."
  :type 'boolean
  :group 'tsx-mode)

(defcustom tsx-mode-fold-tree-queries
  ;; TODO: figure out how to not require these to be named, since the names don't
  ;; actually matter
  '("((statement_block) @fb)"
    "(call_expression (template_string) @ts)")
  "List of tree-sitter queries for which to create Origami code-folding nodes.
Each query should point to a tree-sitter node and should provide a name for the
node to be folded (though the value of that name doesn't matter)"
  :type '(repeat string)
  :group 'tsx-mode)


(defconst tsx-mode--lib-dir
  (file-name-directory (locate-library "tsx-mode.el"))
  "Internal constant.
Location on disk of the directory containing this library.")

(defconst tsx-mode--filename-for-platform
  (cond
   ((eq system-type "windows")
    "windows.tar.gz")
   ((eq system-type "darwin")
    ;; TODO: M1 mac support once the binaries are built
    "macos.tar.gz")
   (t "linux.tar.gz"))
  "Internal constant.
Returns the name of a shared-library archive appropriate for the current OS
and hardware.")


(defvar-local tsx-mode-debug
    nil
  "Debug boolean for tsx-mode.  Causes a bunch of helpful(?) text to be spammed
to *Messages*.")


(defun tsx-mode--debug (&rest args)
  "Internal function.
Print messages only when `tsx-mode-debug` is `t` in this buffer."
  (when tsx-mode-debug
    (apply 'message args)))


(defun tsx-mode-download-and-unpack (parser remote-dir)
  "Internal function.
Downloads and unpacks a tarball containing a tree-sitter shared library.  PARSER
is a symbol describing the parser to load, and REMOTE-DIR is the remote directory
containing it."
  (let* ((filename tsx-mode--filename-for-platform)
         (remote-url (concat remote-dir filename))
         (basename (symbol-name parser))
         (archive-name
          (concat basename "-" filename))
         (destination-path
          (file-name-concat
           tsx-mode--lib-dir
           archive-name)))
    ;; `message' here, not `tsx-mode--debug' - we definitely want to inform the
    ;; user that we're downloading something from the internet
    (message "Fetching %s parser from %s..." basename remote-dir)

    (url-copy-file remote-url destination-path t)
    (call-process
     "tar"
     nil nil nil
     "-C" tsx-mode--lib-dir "-zxf" destination-path)))

(defun tsx-mode--do-fontification (beg end)
  "Internal function.
Perform just-in-time text propertization from BEG to END in the current buffer."
  (tsx-mode--debug "fontifying %d-%d" beg end)
  (tree-sitter-hl--highlight-region beg end nil)
  (cond
    (tsx-mode--current-css-region
     (tsx-mode--fontify-current-css-region))
    (tsx-mode--current-gql-region
     (tsx-mode--fontify-current-gql-region))
    (tsx-mode-css-force-highlighting
     (dolist (region tsx-mode--css-regions)
       (let ((tsx-mode--current-css-region region))
         (tsx-mode--fontify-current-css-region))))
    (t nil))
  `(jit-lock-bounds
    ,(min beg (or (plist-get tsx-mode--current-css-region :region-begin) (point-max)))
    . ,(max end (or (plist-get tsx-mode--current-css-region :region-end) (point-min)))))

(defun maybe-object-intervals (obj)
  "Internal function.
Returns whatever (`object-intervals' OBJ) returns if that function exists, and
nil otherwise."
  (if (fboundp 'object-intervals)
      (object-intervals obj)))

(defun tsx-mode--post-command-hook ()
  "Internal function.
A hook function registered at `post-command-hook'."
  (tsx-mode--configure-region-specific-vars))

(defun tsx-mode--indent ()
  "Internal function.
Calculate indentation for the current line."
  (tsi--indent-line-to
   (+
    ;; indentation for typescript tree-sitter node
    (let ((ts-indent (tsi-calculate-indentation
		      'tsi-typescript--get-indent-for
		      'tsi-typescript--get-indent-for-current-line)))
      (tsx-mode--debug "TS indentation: %d" ts-indent)
      ts-indent)
    (let ((gql-indent (tsx-mode--indent-gql-at-point)))
      (tsx-mode--debug "GQL indentation: %d" gql-indent)
      gql-indent)
    (let ((css-indent (tsx-mode--indent-css-at-point)))
      (tsx-mode--debug "CSS indentation: %d" css-indent)
      css-indent))))

(defun tsx-mode--configure-region-specific-vars ()
  "Internal function.
(Re-)configure important variables as point moves around the buffer."
  (tsx-mode--debug "current CSS region: %s, current GQL region: %s, in jsx:"
                   tsx-mode--current-css-region
                   tsx-mode--current-gql-region
                   (tsx-mode--is-in-jsx-p))
  (cond
    (tsx-mode--current-css-region
     (setq comment-start "/* ")
     (setq comment-end " */")
     (setq comment-start-skip "/\\*")
     (setq comment-end-skip "[[:space:]]*\\*/\\n?"))
    ((tsx-mode--is-in-jsx-p)
     (setq comment-start "{/* ")
     (setq comment-end " */}")
     (setq comment-start-skip "{/\\*+[[:space:]]*")
     (setq comment-end-skip "\\*/}\\n?"))
    (t
     ;; TODO: allow configuration of style of non-TSX non-CSS comments?
     (setq comment-start "// ")
     (setq comment-end "")
     (setq comment-start-skip "//[[:space:]]*")
     (setq comment-end-skip nil)))
  (comment-normalize-vars))

(defun tsx-mode--comment-region-function (beg end &optional arg)
  "Internal function.
Wrap `comment-region-default' and do some stuff afterwards without relying on
advice."
  (funcall 'comment-region-default beg end arg)
  (funcall indent-line-function))

(defun tsx-mode--uncomment-region-function (beg end &optional arg)
  "Internal function.
Wrap `uncomment-region-default' and do some stuff afterwards without relying on
advice."
  (funcall 'uncomment-region-default beg end arg)
  (funcall indent-line-function))

(defun tsx-mode--looking-at-node-p (node-type &optional pos)
  "Internal function.
Returns t if POS is inside a tree-sitter node of type NODE-TYPE.  POS defaults
to point."
  (let* ((current-node
	  (tree-sitter-node-at-pos nil (or pos (point))))
	 (current-node-type
	  (when current-node
	    (tsc-node-type current-node))))
    (while (and current-node
		(not (equal
		      node-type
		      (tsc-node-type current-node))))
      (setq current-node
	    (tsc-get-parent current-node)))
    (if current-node
	t
      nil)))

(defun tsx-mode--completion-at-point ()
  "Internal function.
Delegate to either css-mode's capf or lsp-mode's capf depending on where point
is."
  (cond
   ((tsx-mode--looking-at-node-p 'template_substitution)
    (lsp-completion-at-point))
   (tsx-mode--current-css-region
    (tsx-mode--css-completion-at-point))
   (tsx-mode--current-gql-region
    (tsx-mode--gql-completion-at-point))
   (t
    (lsp-completion-at-point))))

(defun tsx-mode--make-captures-tree (captures create start end)
  "Internal function.
Make a tree from CAPTURES using CREATE that are between START and END.  The
CAPTURES, as returned by `tsc-query-captures', may overlap, thus this generates
a tree (or multiple) as required by Origami.

Returns a pair (regions . captures) with the remaining captures."
  (let (regions break)
    (while (and (not break) captures)
      (let* ((elt (cdar captures))
             (e-start (tsc-node-start-position elt))
             (e-end (tsc-node-end-position elt)))
        (if (>= e-start end)
            ;; the current capture is outside of our range, we're done
            ;; here
            (setq break t)
          (let* ((e-tree (tsx-mode--make-captures-tree
                          (cdr captures)
                          create
                          e-start
                          e-end))
                 (e-children (car e-tree)))
            (setq captures (cdr e-tree))
            (setq regions
                  (cons
                   (funcall create
                            e-start
                            e-end
                            0
                            e-children)
                   regions))))))
    (cons (reverse regions) captures)))

(defun tsx-mode--origami-parser (create)
  "Internal function.
Returns a parser for origami.el code folding.  The parser must return a list of
fold nodes, where each fold node is created by invoking CREATE."
  (lambda (content)
    (let* ((query (tsc-make-query
                   (tsc-tree-language tree-sitter-tree)
                   (string-join tsx-mode-fold-tree-queries)))
           (captures (tsc-query-captures
                      query
                      (tsc-root-node tree-sitter-tree)
                      (lambda (beg end)
                        (buffer-substring
                         (byte-to-position beg)
                         (byte-to-position end))))))
      (car
       (tsx-mode--make-captures-tree
        ;; captures is an unspecified sequence (currently array) but
        ;; we'd prefer to process it as a list
        (mapcar #'identity captures)
        create
        (point-min)
        (point-max))))))

(defun tsx-mode--tsx-self-closing-tag-at-point-p ()
  "Internal function.
Return t if a self-closing tag is allowed to be inserted at point."
  (save-excursion
    (tsx-mode--debug "checking current named node %s for self-closing tag support..."
                     (when (tree-sitter-node-at-pos :named) (tsc-node-type (tree-sitter-node-at-pos :named))))
    (re-search-backward "[^\r\n[:space:]]" nil t)
    (let* ((last-named-node (tree-sitter-node-at-pos :named))
           (last-named-node-type (when last-named-node (tsc-node-type last-named-node)))
           (last-anon-node (tree-sitter-node-at-pos :anonymous))
           (last-anon-node-type (when last-anon-node (tsc-node-text last-anon-node))))
      (tsx-mode--debug "checking named node %s and anon node %s for self-closing tag support..."
               last-named-node-type last-anon-node-type)
       (or (string= last-anon-node-type "=>")
           (string= last-anon-node-type "(")
           (string= last-anon-node-type "?")
           (string= last-anon-node-type ":")
           (string= last-anon-node-type "[")
           (string= last-anon-node-type ",")
           (string= last-anon-node-type "=")
           (eq last-named-node-type 'jsx_opening_element)
           (eq last-named-node-type 'jsx_closing_element)
           (eq last-named-node-type 'jsx_fragment)
           (eq last-named-node-type 'jsx_expression)))))

(defun tsx-mode-tsx-maybe-insert-self-closing-tag ()
  "When `tsx-mode-tsx-auto-tags' is non-nil, insert a self-closing element
instead of a plain '<' character (where it makes sense to)."
  (interactive)
  (if (or (bobp)
          (and tsx-mode-tsx-auto-tags
               (tsx-mode--tsx-self-closing-tag-at-point-p)))
      (progn
        (insert "</>")
        (backward-char 2))
    (insert "<")))

(defun tsx-mode-tsx-maybe-close-tag ()
  "When `tsx-mode-tsx-auto-tags' is non-nil, turn the current self-closing tag
(if any) into a regular tag instead of inserting a plain '>' character."
  (interactive)
  (if (and tsx-mode-tsx-auto-tags
           (tsx-mode--tsx-tag-convert-at-point-p))
      (let* ((node-element-name
              (save-excursion
                (goto-char (tsc-node-start-position (tree-sitter-node-at-point :named)))
                ;; TODO: there should be a way to use [:word:] here right?
                (re-search-forward "<\\([-a-zA-Z0-9$_.]+\\)" nil t)
                (tsx-mode--debug "tag match: %s" (match-data))
                (match-string 1)))
             ;; the above will calculate the name of a fragment as "/"
             (str (format "></%s>" (if (string= node-element-name "/") "" node-element-name))))
        (re-search-forward "/>" nil t)
        (delete-char -2)
        (insert str)
        (backward-char (- (length str) 1)))
    (insert ">")))

(defun tsx-mode--tsx-tag-convert-at-point-p ()
  "Internal function.
Return t if a self-closing tag at point can be turned into an opening tag and a
closing tag."
  (or
   (when-let* ((current-named-node (tree-sitter-node-at-pos :named))
               (current-named-node-type (tsc-node-type current-named-node)))
     ;; self-closing tags can be turned into regular tag sets
     (eq current-named-node-type 'jsx_self_closing_element))
   (save-excursion
     ;; a "</>" string inserted via `tsx-mode-auto-tags' can be turned into
     ;; a set of JSX fragment tags
     (backward-char 1)
     (looking-at-p "</>"))))

(defun tsx-mode--is-in-jsx-p (&optional maybe-pos)
  "Internal function.
Return t if MAYBE-POS is inside a JSX-related tree-sitter node.  MAYBE-POS
defaults to (`point') if not provided."
  (let ((pos (or maybe-pos (point))))
    (and-let* ((current-node (tree-sitter-node-at-pos :named pos))
               (current-node-type (tsc-node-type current-node))
               (is-jsx (or (eq current-node-type 'jsx_expression)
                           (eq current-node-type 'jsx_self_closing_element)
                           (eq current-node-type 'jsx_text)))))))

(defun tsx-mode-coverage-toggle ()
  "Toggles code-coverage overlay."
  (interactive)
  (if coverlay-minor-mode
      (coverlay-minor-mode 'toggle)
    (let* ((package-json
	    (locate-dominating-file
	     (buffer-file-name (current-buffer))
	     "package.json"))
	   (base-path
	    (when package-json
	      (expand-file-name package-json)))
	   (coverage-file
	    (when base-path
	      (concat
	       base-path
	       "coverage/lcov.info"))))
      (setq-local coverlay:base-path base-path)
      (when (and coverage-file
		 (file-exists-p coverage-file)) ; can't handle nil
	(coverlay-watch-file coverage-file))
      (coverlay-minor-mode 'toggle))))

(defun tsx-mode-get-parser (parser remote-dir)
  "Attempts to load the provided parser specified by the PARSER symbol, first
by checking locally then by downloading a platform-specific shared library
located at REMOTE-DIR.  Returns t if successful, nil otherwise."
  (or
   (condition-case nil
       (tree-sitter-load parser)
     (error nil)
     (:success t))
   (condition-case nil
       (progn
         (tsx-mode-download-and-unpack parser remote-dir)
         (add-to-list 'tree-sitter-load-path tsx-mode--lib-dir))
     (error nil)
     (:success t))))

(defun tsx-mode--setup-buffer ()
  "Internal function.
Hook to be called to finish configuring the current buffer after lsp-mode has
been enabled."
  ;; set up tree-sitter and related
  (tree-sitter-require 'tsx)
  (add-to-list
   'tree-sitter-major-mode-language-alist
   '(tsx-mode . tsx))
  (setq tree-sitter-hl-default-patterns
        (tree-sitter-langs--hl-default-patterns 'tsx))
  (tsi-typescript-mode)
  (tree-sitter-hl-mode)
  ;; set up code-folding
  (origami-mode t)
  (add-to-list
   'origami-parser-alist
   '(tsx-mode . tsx-mode--origami-parser))
  (set (make-local-variable 'comment-use-syntax) nil)
  (set (make-local-variable 'comment-region-function)
       'tsx-mode--comment-region-function)
  (set (make-local-variable 'uncomment-region-function)
       'tsx-mode--uncomment-region-function)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-end-skip)
  (make-local-variable 'indent-line-function)
  (setq-local indent-line-function 'tsx-mode--indent)
  (jit-lock-register
   'tsx-mode--do-fontification)
  (add-hook
   'post-command-hook
   'tsx-mode--post-command-hook
   nil t)
  (add-hook
   'jit-lock-functions
   'tsx-mode--do-fontification
   nil t)
  (setq
   completion-at-point-functions
   '(tsx-mode--completion-at-point lsp-completion-at-point))
  (tsx-mode-css)
  (tsx-mode-gql))


;;;###autoload
(define-derived-mode
    tsx-mode prog-mode "TSX"
    "A batteries-included major mode for JSX and friends."
    :group 'tsx-mode
    :syntax-table (let ((table (make-syntax-table)))
                    (c-populate-syntax-table table)
                    ;; backticks are string delimiters
                    (modify-syntax-entry ?` "\"" table)
                    ;; dollar signs are allowed in symbol names
                    (modify-syntax-entry ?$ "_" table)
                    table)

    (define-key
     tsx-mode-map
     (kbd "C-c t f")
     'origami-toggle-node)
    (define-key
     tsx-mode-map
     (kbd "C-c t F")
     'origami-toggle-all-nodes)
    (define-key
     tsx-mode-map
     (kbd "C-c t c")
     'tsx-mode-coverage-toggle)
    (define-key
     tsx-mode-map
     (kbd "<")
     'tsx-mode-tsx-maybe-insert-self-closing-tag)
    (define-key
     tsx-mode-map
     (kbd ">")
     'tsx-mode-tsx-maybe-close-tag)
    ;; configure things after lsp-mode is finished doing whatever it does
    (add-hook
     'lsp-completion-mode-hook
     'tsx-mode--setup-buffer
     100 t)
    (lsp-ensure-server 'ts-ls)
    (lsp)
    (lsp-completion-mode t))


(when load-file-name
  (let ((tsx-mode-dir (file-name-directory load-file-name)))
    (message "tsx-mode loading files from %s" tsx-mode-dir)
    (message "tree-sitter load path is %s" tree-sitter-load-path)
    (load-file (concat tsx-mode-dir "graphql.el"))
    (load-file (concat tsx-mode-dir "css-in-js.el"))))


(provide 'tsx-mode)

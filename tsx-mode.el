;;; tsx-mode.el --- a batteries-included major mode for JSX and friends -*- lexical-binding: t -*-

;;; Version: 1.9.1

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsx-mode.el

;;; Package-Requires: ((emacs "27") (tsi "1.0.0") (tree-sitter-langs "0.11.3") (lsp-mode "8.0.0") (origami "1.0"))

;;; Code:

(require 'css-mode)
(require 'js)
(require 'seq)

(require 'lsp)
(require 'lsp-completion)
;; origami depends on some now-deprecated cl functions and there's not much we
;; can do about that
(let ((byte-compile-warnings '((not cl-functions))))
  (require 'origami))
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tsi-css)
(require 'tsi-typescript)

(unless (fboundp 'object-intervals)
  (message "tsx-mode CSS fontification requires Emacs 28.1+, so we will do without it"))

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


(defcustom tsx-mode-css-force-highlighting nil
  "When set to t, CSS-in-JS tagged-template strings will have syntax highlighting
applied to them even if point is no longer inside of them."
  :type 'boolean
  :group 'tsx-mode)


(defvar tsx-mode-css-region-delimiters
  '((;; styled-components, emotion, etc.
     :start "\\(styled\\|css\\|keyframes\\|createGlobalStyle\\)[.()<>[:alnum:]]?+`"
     :start-offset 0
     :end "`;"
     :end-offset -1)
    (;; styled-jsx
     :start "<style jsx[[:blank:][:alnum:]]?+>{`"
     :start-offset 0
     :end "`}"
     :end-offset -1)
    (;; astroturf
     :start "stylesheet`"
     :start-offset 0
     :end "`;"
     :end-offset -1)
    (;; `css` react prop
     :start "{css`"
     :start-offset 0
     :end "`}"
     :end-offset -1))
  "A list of information defining CSS-in-JS regions.

Each CSS-in-JS mode definition is a plist containing the following properties:

:start - a string defining a regular expression for finding the beginning of a
         region
:start-offset - a number defining the offset from the end of :start at which the
                region should begin
:end - a string defining a regular expression for finding the end of a region
:end-offset - a number defining the offset from the end of :end at which the
               region should end.")


(defvar-local tsx-mode-css-enter-region-hook
    nil
  "A hook which gets run when point is leaving a CSS-in-JS region.")


(defvar-local tsx-mode-css-exit-region-hook
    nil
  "A hook which gets run when point is entering a CSS-in-JS region.")


(defvar-local tsx-mode-debug
    nil
  "Debug boolean for tsx-mode.  Causes a bunch of helpful(?) text to be spammed
to *Messages*.")


(defun tsx-mode--debug (&rest args)
  "Internal function.

Print messages only when `tsx-mode-debug` is `t` in this buffer."
  (when tsx-mode-debug
    (apply 'message args)))


(defvar tsx-mode--css-buffer
    nil
  "Internal variable.

Super secret buffer for performing CSS-related tasks.")


(defvar-local tsx-mode--current-css-region
    nil
  "Internal variable.

CSS-in-JS region containing point (if any).")


(defvar-local tsx-mode--css-regions
    '()
  "Internal variable.

Plist of all CSS-in-JS regions in this buffer.")


(defun tsx-mode--css-inline-style-at-pos-p (pos)
  "Internal function.

Return t if we think the CSS-in-JS region at POS is written as an inline JSX
style prop."
  (when-let* ((current-node (tree-sitter-node-at-pos :named pos))
              (parent-node (tsc-get-parent current-node))
              (grandparent-node (tsc-get-parent parent-node)))
    (and
     (eq (tsc-node-type parent-node) 'call_expression)
     (eq (tsc-node-type grandparent-node) 'jsx_expression))))


(defun tsx-mode--css-get-regions-for-def (region-def)
  "Internal function.

Find and return CSS-in-JS regions in this buffer defined by REGION-DEF."
  (let ((regions-for-def '()))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while
            (re-search-forward
             (plist-get region-def :start)
             nil t)
          (let ((start-pos (point))
                (end-pos (re-search-forward
                          (plist-get region-def :end)
                          nil t)))
            (push
             (list :region-begin (+ start-pos (plist-get region-def :start-offset))
                   ;; as a convenience, (point-max) can also end a region
                   :region-end (if end-pos
                                   (+ end-pos
                                      (plist-get region-def :end-offset))
                                 (point-max))
                   :inline-style (tsx-mode--css-inline-style-at-pos-p
                                  (+ start-pos (plist-get region-def :start-offset))))
           regions-for-def)))))
    regions-for-def))


(defun tsx-mode--css-update-regions ()
  "Internal function.

Update the list of CSS-in-JS regions in the current buffer, and optionally call
for re-fontification if things have changed a lot."
  (let ((next-region-list
         (apply
          'append
          (mapcar
           'tsx-mode--css-get-regions-for-def
           tsx-mode-css-region-delimiters))))
    (tsx-mode--debug "regions: %s" next-region-list)
    (when (not (eq (length next-region-list)
                   (length tsx-mode--css-regions)))
      ;; TODO: this check isn't as smart as it could be
      (jit-lock-refontify (point-min) (point-max)))
    (setq tsx-mode--css-regions next-region-list)))


(defun tsx-mode--css-region-for-point ()
  "Internal function.

Get the region at point (if any)."
  (seq-find
   (lambda (elt)
     (and
      (>= (point) (plist-get elt :region-begin))
      (< (point) (plist-get elt :region-end))))
   tsx-mode--css-regions
   nil))


(defun tsx-mode--css-region-for-line ()
  "Internal function.

Get the region beginning on, ending on, or including the line number at point
(if any)."
  (or
   tsx-mode--current-css-region
   (seq-find
    (lambda (elt)
      (and (>= (line-number-at-pos)
               (line-number-at-pos (plist-get elt :region-start)))
           (<= (line-number-at-pos)
               (line-number-at-pos) (plist-get elt :region-end))))
    tsx-mode--css-regions)))


(defun tsx-mode--do-fontification (beg end)
  "Internal function.

Perform just-in-time text propertization from BEG to END in the current buffer."
  (tsx-mode--debug "fontifying %d-%d" beg end)
  (tree-sitter-hl--highlight-region beg end nil)
  (cond
    (tsx-mode--current-css-region
     (tsx-mode--fontify-current-css-region))
    (tsx-mode-css-force-highlighting
     (dolist (region tsx-mode--css-regions)
       (let ((tsx-mode--current-css-region region))
         (tsx-mode--fontify-current-css-region))))
    (t nil))
  `(jit-lock-bounds
    ,(min beg (or (plist-get tsx-mode--current-css-region :region-begin) (point-max)))
    . ,(max end (or (plist-get tsx-mode--current-css-region :region-end) (point-min)))))


(defun maybe-object-intervals (obj)
  (if (fboundp 'object-intervals)
      (object-intervals obj)))

(defun tsx-mode--fontify-current-css-region ()
  "Internal function.

Perform syntax highlighting of CSS in a separate buffer then copy text
properties back to this buffer."
  (let* ((region tsx-mode--current-css-region)
         (beg (max (point-min) (plist-get region :region-begin)))
         (end (min (point-max) (plist-get region :region-end)))
         (str (buffer-substring beg (- end 1)))
         (fontified-text-properties-list nil))
    ;; get fontification properties to apply by font-locking our secret buffer
    (with-current-buffer tsx-mode--css-buffer
      ;; --fontify-current-css-region is called in the context of a post-command
      ;; hook which means `inhibit-modification-hooks' is temporarily set to non-
      ;; nil.  but that will prevent desirable side-effects from occurring in our
      ;; CSS buffer so turn it off for a little while
      (let ((inhibit-modification-hooks nil))
        (erase-buffer)
        ;; wrap the inserted text in a dummy CSS selector.  this allows us to
        ;; properly calculate indentation as well as get capf to return
        ;; everything we want it to
        (insert (format "div{%s}" str))
        (tree-sitter--after-change (point-min) (point-max) 0)
        (font-lock-ensure (point-min) (point-max)))
      (setq fontified-text-properties-list
            (maybe-object-intervals
             (buffer-substring
              (+ (length "div{") (point-min))
              (- (point-max) (length "}"))))))
    ;; apply those fontification properties to this buffer
    (with-silent-modifications
      (dolist (range-with-property fontified-text-properties-list)
        (set-text-properties
         (+ beg (elt range-with-property 0))
         (+ beg (elt range-with-property 1))
         (elt range-with-property 2))))))


(defun tsx-mode--post-command-hook ()
  "Internal function.

A hook function registered at `post-command-hook'."
  (tsx-mode--update-current-css-region)
  (tsx-mode--configure-region-specific-vars))


(defun tsx-mode--do-css-region-change (old-region new-region)
  "Internal function.

Run the exit-CSS-region hook with OLD-REGION, then the enter-CSS-region hook
with NEW-REGION, then returns NEW-REGION."
  (unless (or (=
               (plist-get new-region :region-begin)
               (plist-get old-region :region-begin))
              (=
               (plist-get new-region :region-end)
               (plist-get new-region :region-end)))
    ;; don't run hooks if the region is the same but its bounds have changed
    (tsx-mode--debug "changing css-in-js regions")
    (run-hook-with-args 'tsx-mode-css-exit-region-hook old-region)
    (run-hook-with-args 'tsx-mode-css-enter-region-hook new-region))
  new-region)


(defun tsx-mode--do-css-region-enter (new-region)
  "Internal function.

Run the enter-CSS-region hook with NEW-REGION, then returns NEW-REGION."
  (tsx-mode--debug "entering css-in-js region")
  (run-hook-with-args 'tsx-mode-css-enter-region-hook new-region)
  new-region)


(defun tsx-mode--do-css-region-exit (old-region)
  "Internal function.

Run the exit-CSS-region hook with OLD-REGION, then returns OLD-REGION."
  (tsx-mode--debug "exiting css-in-js region")
  (run-hook-with-args 'tsx-mode-css-exit-region-hook old-region)
  old-region)


(defun tsx-mode--update-current-css-region ()
  "Internal function.

Detect changes to the current CSS-in-JS region, and update state and run hooks
if necessary."
  (setq
   tsx-mode--current-css-region
   (let ((old-region tsx-mode--current-css-region)
         (new-region (tsx-mode--css-region-for-point)))
     (tsx-mode--debug "old region: %s new region: %s" old-region new-region)
     (cond
       ((and old-region new-region)
        (tsx-mode--do-css-region-change old-region new-region)
        new-region)
       (new-region
        (tsx-mode--do-css-region-enter new-region)
        new-region)
       (old-region
        (tsx-mode--do-css-region-exit old-region)
        nil)
       (t nil)))))


(defun tsx-mode--after-change-function (beg end old-text-length)
  "Internal function.

A hook function registered at `after-change-functions'."
  (tsx-mode--css-update-regions)
  (tsx-mode--update-current-css-region)
  (when tsx-mode--current-css-region
    (tsx-mode--fontify-current-css-region)))


(defun tsx-mode--indent-css-at-pos (css-buffer-pos)
  "Internal function.

Calculate indentation for line CSS-BUFFER-POINT in the CSS-in-JS buffer."
  (tsi--indent-line-to
   ;; if a CSS region has the :inline-style property then we want to apply
   ;; an extra amount of indentation equal to the parent prop's indentation
   (+
    (with-current-buffer tsx-mode--css-buffer
      ;;     (setq-local tsi-debug t)
      (goto-char css-buffer-pos)
      (tsi--walk 'tsi-css--get-indent-for))
    (if (plist-get tsx-mode--current-css-region :inline-style)
        (save-excursion
          (goto-char (plist-get tsx-mode--current-css-region :region-begin))
          (tsi--walk 'tsi-typescript--get-indent-for))
      0))))


(defun tsx-mode--indent-css ()
  "Internal function.

Calculate indentation for the current line."
  (if (< (save-excursion
           (beginning-of-line)
           (point))
         (plist-get tsx-mode--current-css-region :region-begin))
      ;; point is in a CSS region but the line itself is not
      (tsi-typescript--indent-line)
    (tsx-mode--indent-css-at-pos
     (+ 1 (length "div{") (- (point) (plist-get tsx-mode--current-css-region :region-begin))))))


(defun tsx-mode--css-enter-region (new-region)
  "Internal function.

A hook function registered at `tsx-mode-css-enter-region-hook'."
  ;; don't forget to bounds-check in case the region has shrunk due to a kill
  (jit-lock-refontify
   (min (plist-get new-region :region-begin) (point-max))
   (min (plist-get new-region :region-end) (point-max))))


(defun tsx-mode--css-exit-region (old-region)
  "Internal function.

A hook function registered at `tsx-mode-css-exit-region-hook'."
  ;; don't forget to bounds-check in case the region has shrunk due to a kill
  (jit-lock-refontify
   (min (plist-get old-region :region-begin) (point-max))
   (min (plist-get old-region :region-end) (point-max))))


(defun tsx-mode--configure-region-specific-vars ()
  "Internal function.
(Re-)configure important variables as point moves around the buffer."
  (tsx-mode--debug "current CSS region: %s, in jsx:"
                   tsx-mode--current-css-region
                   (tsx-mode--is-in-jsx-p))
  (cond
    (tsx-mode--current-css-region
     (setq-local indent-line-function 'tsx-mode--indent-css)
     (setq comment-start "/* ")
     (setq comment-end " */")
     (setq comment-start-skip "/\\*")
     (setq comment-end-skip "[[:space:]]*\\*/\\n?"))
    ((tsx-mode--is-in-jsx-p)
     (setq indent-line-function 'tsi-typescript--indent-line)
     (setq comment-start "{/* ")
     (setq comment-end " */}")
     (setq comment-start-skip "{/\\*+[[:space:]]*")
     (setq comment-end-skip "\\*/}\\n?"))
    (t
     (setq indent-line-function 'tsi-typescript--indent-line)
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


(defun tsx-mode--completion-at-point ()
  "Internal function.

Delegate to either css-mode's capf or lsp-mode's capf depending on where point
is."
  (if tsx-mode--current-css-region
      (let* ((point-offset
              (+ 1
                 (length "div{")
                 (- (point) (plist-get tsx-mode--current-css-region :region-begin))))
             (completion
              (with-current-buffer tsx-mode--css-buffer
                (goto-char point-offset)
                (css-completion-at-point))))
        (if completion
            (let ((offset (+ (plist-get tsx-mode--current-css-region :region-begin)
                             (- (+ 1 (length "div{"))))))
              ;; translate css-buffer coordinates into main-buffer coordinates
              (setcar (nthcdr 1 completion)
                      (+ (cadr completion) offset))
              (setcar (nthcdr 0 completion)
                      (+ (car completion) offset))
              completion)
          nil))
    (lsp-completion-at-point)))


(defun tsx-mode--origami-parser (create)
  "Internal function.

Parser for origami.el code folding.  Must return a list of fold nodes, where
each fold node is created by invoking CREATE."
  (lambda (content)
    ;; assume `content` is equal to the current buffer contents, so we can re-
    ;; use our existing list of CSS-in-JS regions.  is that safe?  dunno!
    (mapcar
     (lambda (el)
       ;; TODO: this -1 offset might need to be specific to a given region type
       ;; (e.g. styled-components)
       (funcall create
                (plist-get el :region-begin)
                (plist-get el :region-end)
                -1 nil))
     tsx-mode--css-regions)))


(defun tsx-mode-css-toggle-fold ()
  "Toggle code-folding for the CSS-in-JS region belonging to the current line."
  (interactive)
  (when-let ((region-to-fold (tsx-mode--css-region-for-line)))
    (origami-toggle-node (current-buffer) (plist-get region-to-fold :region-begin))))


(defun tsx-mode-css-toggle-fold-all ()
  "Toggle code-folding for all CSS-in-JS regions."
  (interactive)
  (origami-toggle-all-nodes (current-buffer)))


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
           (eq last-named-node-type 'jsx_opening_element)))))


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


(defun tsx-mode--setup-buffer ()
  "Internal function.

Hook to be called to finish configuring the current buffer after lsp-mode has
been enabled."
  ;; set up tree-sitter and related
  (tree-sitter-require 'tsx)
  (add-to-list
   'tree-sitter-major-mode-language-alist
   '(tsx-mode . tsx))
  (add-to-list
   'tree-sitter-major-mode-language-alist
   '(scss-mode . css))
  (setq tree-sitter-hl-default-patterns
        (tree-sitter-langs--hl-default-patterns 'tsx))
  (tsi-typescript-mode)
  (tree-sitter-hl-mode)
  ;; set up the CSS-in-JS hidden buffer
  (unless tsx-mode--css-buffer
    (tsx-mode--debug "setting up css buffer...")
    (setq tsx-mode--css-buffer
          (get-buffer-create " *tsx-mode css*"))
    (with-current-buffer tsx-mode--css-buffer
      (scss-mode)
      ;; scss-mode's native highlighting is nicer-looking than tree-sitter's
      ;;      (tree-sitter-hl-mode)
      (tsi-css-mode)))
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

  (tsx-mode--css-update-regions)

  (jit-lock-register
   'tsx-mode--do-fontification)
  (add-hook
   'post-command-hook
   'tsx-mode--post-command-hook
   nil t)
  (add-hook
   'after-change-functions
   'tsx-mode--after-change-function
   nil t)
  (add-hook
   'jit-lock-functions
   'tsx-mode--do-fontification
   nil t)
  (add-hook
   'tsx-mode-css-exit-region-hook
   'tsx-mode--css-exit-region
   nil t)
  (add-hook
   'tsx-mode-css-enter-region-hook
   'tsx-mode--css-enter-region
   nil t)
  (add-hook
   'completion-at-point-functions
   'tsx-mode--completion-at-point
   nil t))


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

    (define-key tsx-mode-map
        (kbd "C-c t f") 'tsx-mode-css-toggle-fold)
    (define-key tsx-mode-map
        (kbd "C-c t F") 'tsx-mode-css-toggle-fold-all)
    (define-key tsx-mode-map
        (kbd "<") 'tsx-mode-tsx-maybe-insert-self-closing-tag)
    (define-key tsx-mode-map
        (kbd ">") 'tsx-mode-tsx-maybe-close-tag)

    ;; configure things after lsp-mode is finished doing whatever it does
    (add-hook
     'lsp-completion-mode-hook
     'tsx-mode--setup-buffer
     100 t)
    (lsp-ensure-server 'ts-ls)
    ;; TODO: would a CSS langserver be useful here?
    (lsp)
    (lsp-completion-mode t))


(provide 'tsx-mode)

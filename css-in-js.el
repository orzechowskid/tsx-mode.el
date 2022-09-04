;;; css-in-js.el --- CSS-in-JS support for tsx-mode -*- lexical-binding: t -*-

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsx-mode.el

;;; Code:


(require 'css-mode)
(require 'tree-sitter)
(require 'tsi-css)


(unless (fboundp 'object-intervals)
  (message "tsx-mode CSS fontification requires Emacs 28.1+, so we will do without it"))


(defconst tsx-mode--css-archive
  "https://github.com/orzechowskid/tree-sitter-css-in-js/releases/download/latest/"
  "Location of tarballs containing tree-sitter CSS-in-JS shared libraries.")


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

(defun tsx-mode--css-buffer-sync (region)
  "Internal function.
Ensures hidden buffer is up-to-date with text of the provided region"  
  (let ((str
         (buffer-substring-no-properties
          (max (point-min) (plist-get region :region-begin))
          (min (point-max) (- (plist-get region :region-end) 1)))))
    (with-current-buffer tsx-mode--css-buffer
      (unless (string= str (buffer-string))
        (erase-buffer)
        (insert (format "div{%s}" str))
        (tree-sitter--after-change (point-min) (point-max) 0)
        (font-lock-ensure (point-min) (point-max))))))

(defun tsx-mode--fontify-current-css-region ()
  "Internal function.
Perform syntax highlighting of CSS in a separate buffer then copy text
properties back to this buffer."
  (tsx-mode--debug "fontify css")
  (tsx-mode--css-buffer-sync tsx-mode--current-css-region)
  (let* ((begin (plist-get tsx-mode--current-css-region :region-begin))
         (fontified-text-properties-list
          (with-current-buffer tsx-mode--css-buffer
            (maybe-object-intervals
             (buffer-substring
              (+ (length "div{") (point-min))
              (- (point-max) (length "}")))))))
    ;; apply those fontification properties to this buffer if we have them
    (when fontified-text-properties-list
      (with-silent-modifications
        (dolist (range-with-property fontified-text-properties-list)
          ;; each list entry is '(range-start-pos range-end-pos (plist))
          (set-text-properties
           (+ begin (elt range-with-property 0))
           (+ begin (elt range-with-property 1))
           (elt range-with-property 2)))))))

(defun tsx-mode--do-css-region-change (old-region new-region)
  "Internal function.
Run the exit-CSS-region hook with OLD-REGION, then the enter-CSS-region hook
with NEW-REGION, then returns NEW-REGION."
  (unless (or (=
               (plist-get new-region :region-begin)
               (plist-get old-region :region-begin))
              (=
               (plist-get new-region :region-end)
               (plist-get old-region :region-end)))
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

(defun tsx-mode--indent-css-at-point ()
  "Internal function.
Calculate indentation amount for CSS-in-JS at the current point."
  (if tsx-mode--current-css-region
      (+
       ;; basic tsi-css indentation
       (let ((buffer-offset
              (+ 1 (- (point)
                      (plist-get tsx-mode--current-css-region :region-begin)))))
         (with-current-buffer tsx-mode--css-buffer
           (goto-char (+
                       buffer-offset
                       (length "div{")))
           (tsi--walk 'tsi-css--get-indent-for)))
       ;; if a CSS region has the :inline-style property then we want to apply
       ;; an extra amount of indentation equal to the parent prop's indentation
       (if (plist-get tsx-mode--current-css-region :inline-style)
           (save-excursion
             (goto-char (plist-get tsx-mode--current-css-region :region-begin))
             (tsi--walk 'tsi-typescript--get-indent-for))
         0))
    0))

(defun tsx-mode--css-enter-region (new-region)
  "Internal function.
A hook function registered at `tsx-mode-css-enter-region-hook'."
  (tsx-mode--css-buffer-sync new-region)
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

(defun tsx-mode--css-completion-at-point ()
  "Internal function.
Perform completion-at-point inside the hidden CSS buffer and apply to this one."
  (tsx-mode--debug "CSS completion?")
  (when (and tsx-mode--current-css-region
             (not (tsx-mode--looking-at-node-p 'template_substitution)))
    (let* ((point-offset
            (+ 1
               (length "div{")
               (- (point) (plist-get tsx-mode--current-css-region :region-begin))))
           (completion
            (with-current-buffer tsx-mode--css-buffer
              (goto-char point-offset)
              (css-completion-at-point)))
           (offset (+ (plist-get tsx-mode--current-css-region :region-begin)
                      (- (+ 1 (length "div{"))))))
      ;; translate css-buffer coordinates into main-buffer coordinates
      (setcar (nthcdr 1 completion)
              (+ (cadr completion) offset))
      (setcar (nthcdr 0 completion)
              (+ (car completion) offset))
      completion)))


(define-minor-mode tsx-mode-css
  "A tsx-mode minor mode for CSS-in-JS."
  :delight nil
  :lighter ""
  :group 'tsx-mode
  (cond
   (tsx-mode-css
    (unless tsx-mode--css-buffer
      (tsx-mode--debug "setting up css buffer...")
      (setq tsx-mode--css-buffer
            (get-buffer-create " *tsx-mode css*"))
      (with-current-buffer tsx-mode--css-buffer
        (tsx-mode--css-mode)))
    (add-hook
     'tsx-mode-css-exit-region-hook
     'tsx-mode--css-exit-region
     nil t)
    (add-hook
     'tsx-mode-css-enter-region-hook
     'tsx-mode--css-enter-region
     nil t)
    (add-hook
     'post-command-hook
     'tsx-mode--update-current-css-region
     nil t)
    (add-to-list
     'tsx-mode--indent-fns
     'tsx-mode--indent-css-at-point)
    (add-hook
     'after-change-functions
     (lambda (beg end old-text-length)
       (tsx-mode--css-update-regions)
       (tsx-mode--update-current-css-region))
     nil t)
    (tsx-mode--css-update-regions)
    ;; lsp-mode attempts to place its own completion function at the head of
    ;; this list, but we don't want that
    (add-hook
     'lsp-completion-mode-hook
     (lambda ()
       (remove-hook
        'completion-at-point-functions
        'tsx-mode--css-completion-at-point)
       (add-to-list
        'completion-at-point-functions
        'tsx-mode--css-completion-at-point))))
   (t nil)))


(define-derived-mode
  tsx-mode--css-mode scss-mode "TSX+CSS"
  "Internal mode used only for associating our hidden buffer with a tree-sitter
parser, to avoid clobbering any other user-defined major-mode mappings."
  :group 'tsx-mode
  (tree-sitter-mode t)
  (tsi-css-mode))

;; associate our css buffer's major mode with a css-in-js tree-sitter parser if
;; one is found, or fall back to the css parser if not
(add-to-list
 'tree-sitter-major-mode-language-alist
 (if (tsx-mode-get-parser 'css_in_js tsx-mode--css-archive)
     '(tsx-mode--css-mode . css_in_js)
   '(tsx-mode--css-mode . css)))

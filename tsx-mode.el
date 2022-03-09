;;; tsx-mode.el --- a batteries-included major mode for JSX and friends -*- lexical-binding: t

;;; Package-Requires: ((emacs "28.1") (tsi.el "1.0.0") (tree-sitter-langs "0.11.3"))

;;; Code:


(unless (fboundp 'object-intervals)
  (error "Unsupported: tsx-mode.el requires Emacs 28.1+"))
(setq debug-on-error t)


(require 'js)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tsi-css)
(require 'tsi-typescript)


(defvar tsx-mode-css-region-delimiters
  '((
     ;; styled-components, emotion, etc.
     :start "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
     :start-offset 0
     :end "`;"
     :end-offset -1))
  "A list of information defining CSS-in-JS regions.

Each CSS-in-JS mode definition is a plist containing the following properties:

:start - a string defining a regular expression for finding the beginning of a region
:start-offset - number defining the offset from the end of :start at which the region should begin
:end - a string defining a regular expression for finding the end of a region
:end-offset - number defining the offset from the end of :end at which the region should end.")


(defvar-local tsx-mode-css-enter-region-hook
    nil
  "A hook which gets run when point is leaving a CSS-in-JS region.")


(defvar-local tsx-mode-css-exit-region-hook
    nil
  "A hook which gets run when point is entering a CSS-in-JS region.")


(defvar-local tsx-mode-debug
    nil
  "Debug boolean for tsx-mode.  Causes a bunch of helpful(?) text to be spammed to *Messages*.")


(defun tsx-mode--debug (&rest args)
  "Internal function.

Print messages only when `tsx-mode-debug` is `t`."
  (when tsx-mode-debug
    (apply 'message args)))


(defvar-local tsx-mode--css-buffer
    nil
  "Internal variable.

Super secret buffer for performing CSS-related text manipulation.")


(defvar-local tsx-mode--current-css-region
    nil
  "Internal variable.

CSS-in-JS region containing point (if any).")


(defvar-local tsx-mode--css-regions
    '()
  "Internal variable.

List of all CSS-in-JS regions in this buffer.")


(defun tsx-mode--find-css-regions (region-def)
  "Internal function.

Find CSS-in-JS regions defined by REGION-DEF and adds them to `tsx-mode--css-regions."
  (goto-char (point-min))
  (while
      (re-search-forward
       (plist-get region-def :start)
       nil t)
    (let ((start-pos (point)))
      (when
          (re-search-forward
           (plist-get region-def :end)
           nil t)
        (push
         (cons
          (+ start-pos (plist-get region-def :start-offset))
          (+ (point) (plist-get region-def :end-offset)))
         tsx-mode--css-regions)))))


(defun tsx-mode--css-parse-buffer ()
  "Internal function.

Parse the buffer from top to bottom for each entry in the region-definition list."
  (setq tsx-mode--css-regions '())
  (save-excursion
    (dolist (region-def tsx-mode-css-region-delimiters)
      (tsx-mode--find-css-regions region-def)))
  (tsx-mode--debug "regions: %s" tsx-mode--css-regions))


(defun tsx-mode--css-region-for-point ()
  "Internal function.

Gets the region at point (if any)."
  (seq-find
   (lambda (elt)
     (and
      (>= (point) (car elt))
      (< (point) (cdr elt))))
   tsx-mode--css-regions
   nil))


(defun tsx-mode--do-fontification (beg end)
  "Internal function.

Perform just-in-time text propertization."
  (tsx-mode--debug "start fontification: %d-%d" beg end)
  (tree-sitter-hl--highlight-region beg end nil)
  (when tsx-mode--current-css-region
    (tsx-mode--fontify-current-css-region))
  `(jit-lock-bounds
    ,(min beg (or (car tsx-mode--current-css-region) (point-max)))
    . ,(max end (or (cdr tsx-mode--current-css-region) (point-min)))))


(defun tsx-mode--fontify-current-css-region ()
  "Internal function.

Farm out syntax highlighting of CSS to a separate buffer."
  (when tsx-mode--current-css-region
    (tsx-mode--debug "fontifying current region")
    (let* ((beg (car tsx-mode--current-css-region))
           (end (cdr tsx-mode--current-css-region))
           (str (buffer-substring beg (- end 1)))
           (fontified-text-properties-list nil))
      ;; get fontification properties to apply by font-locking our secret buffer
      (with-current-buffer tsx-mode--css-buffer
        (erase-buffer)
        (insert (format "div{%s}" str))
        (font-lock-fontify-buffer)
        (setq fontified-text-properties-list (object-intervals
                                              (buffer-substring
                                               (+ (length "div{") 1)
                                               (- (point-max) (length "}"))))))
      ;; apply those fontification properties to this buffer
      (with-silent-modifications
        (dolist (range-with-property fontified-text-properties-list)
          (set-text-properties
           (+ beg (elt range-with-property 0))
           (+ beg (elt range-with-property 1))
           (elt range-with-property 2)))))))


(defun tsx-mode--post-command-hook ()
  "Internal function.

A hook function registered at `post-command-hook'."
  (tsx-mode--update-current-css-region))


(defun tsx-mode--update-current-css-region ()
  "Internal function.

Detect changes to the current CSS-in-JS region, and update state and run hooks if necessary."
  (let ((old-region tsx-mode--current-css-region)
        (new-region (tsx-mode--css-region-for-point)))
    (if new-region
        (if old-region
            (unless
                (and
                 (>= (point) (car old-region))
                 (< (point) (cdr old-region)))
              ;; jumping from one sub-region to another, but had to first check
              ;; to make sure we weren't jumping from place to place inside the
              ;; same sub-region.
              ;; n.b. I'm almost positive the current check is insufficient
              (save-excursion
                (tsx-mode--debug "changing css-in-js regions")
                (run-hook-with-args 'tsx-mode-css-exit-region-hook old-region)
                (setq tsx-mode--current-css-region new-region)
                (run-hook-with-args 'tsx-mode-css-enter-region-hook new-region)))
          ;; now entering a sub-region
          (save-excursion
            (tsx-mode--debug "entering a css-in-js region")
            (setq tsx-mode--current-css-region new-region)
            (run-hook-with-args 'tsx-mode-css-enter-region-hook new-region)))
      ;; now exiting a sub-region
      (when tsx-mode--current-css-region
        (save-excursion
          (tsx-mode--debug "exiting a css-in-js region")
          (run-hook-with-args 'tsx-mode-css-exit-region-hook old-region)
          (setq tsx-mode--current-css-region nil))))))


(defun tsx-mode--after-change-function (beg end old-text-length)
  "Internal function.

A hook function registered at `after-change-functions'."
  (tsx-mode--css-parse-buffer))


(defun tsx-mode--css-enter-region (new-region)
  "Internal function.

A hook function registered at `tsx-mode-css-enter-region-hook'."
   (jit-lock-refontify (car new-region) (cdr new-region)))


(defun tsx-mode--css-exit-region (old-region)
  "Internal function.

A hook function registered at `tsx-mode-css-exit-region-hook'."
   (jit-lock-refontify (car old-region) (cdr old-region)))


(defun tsx-mode--completion-at-point ()
  "Internal function.

Proxy to either scss-mode's capf or lsp-mode's capf depending on where point is."
  (if tsx-mode--current-css-region
      (css-completion-at-point)
    (lsp-completion-at-point)))


(defun tsx-mode--setup ()
  "Internal function.

Perform initial setup of tsx-mode."
  (tsi-typescript-mode)
  (tree-sitter-hl-mode)
  ;; TODO: this is deprecated in favor of (lsp-mode) but I couldn't get that to work
  (lsp)
  ;; re-use the existing TS[X] data shipped with tree-sitter
  (setq tree-sitter-hl-default-patterns (tree-sitter-langs--hl-default-patterns 'tsx))

  (tsx-mode--css-parse-buffer)

  (unless tsx-mode--css-buffer
    (tsx-mode--debug "setting up css buffer...")
    (setq tsx-mode--css-buffer (get-buffer-create " *tsx-mode css*"))
    (with-current-buffer tsx-mode--css-buffer
      (scss-mode)
      (tsi-css-mode)))

  (jit-lock-register
   #'tsx-mode--do-fontification)

  (add-hook
   'post-command-hook
   #'tsx-mode--post-command-hook
   nil t)
  (add-hook
   'after-change-functions
   #'tsx-mode--after-change-function
   nil t)
  (add-hook
   'jit-lock-functions
   #'tsx-mode--do-fontification
   nil t)
  (add-hook
   'tsx-mode-css-exit-region-hook
   #'tsx-mode--css-exit-region
   nil t)
  (add-hook
   'tsx-mode-css-enter-region-hook
   #'tsx-mode--css-enter-region
   nil t)
  (setq-local completion-at-point-functions nil)
  (add-hook
   'completion-at-point-functions 
   #'tsx-mode--completion-at-point
   nil t)
  
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil)))


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
    (setq tsx-mode-debug t)
    (tsx-mode--setup))


;;;###autoload
(with-eval-after-load 'tree-sitter-langs
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx)))


;;;###autoload
(with-eval-after-load 'lsp-mode
  (add-to-list
   'lsp-language-id-configuration
   '(tsx-mode . "typescript")))


(provide 'tsx-mode)

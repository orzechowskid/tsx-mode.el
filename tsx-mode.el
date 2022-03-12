;; (add-to-list 'load-path "~/.emacs.d/lisp/")
;; (setq straight-check-for-modifications '(check-on-save find-when-checking))
;; (setq straight-vc-git-default-clone-depth 1)
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))
;; (mapc
;;  #'straight-use-package
;;  '(lsp-mode
;;    tree-sitter
;;    tree-sitter-langs
;;    '(tsi :type git :host github :repo "orzechowskid/tsi.el")))

;;; tsx-mode.el --- a batteries-included major mode for JSX and friends -*- lexical-binding: t

;;; Package-Requires: ((emacs "28.1") (tsi.el "1.0.0") (tree-sitter-langs "0.11.3"))

;;; Code:


(unless (fboundp 'object-intervals)
  (error "Unsupported: tsx-mode.el requires Emacs 28.1+"))
(setq debug-on-error t)


(require 'js)
(require 'lsp-mode)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tsi)
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
  (message "new list of parsed regions: %s" tsx-mode--css-regions))


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

Perform just-in-time text propertization from BEG to END in the current buffer."
  (message "start fontification: %d-%d" beg end)
  (tree-sitter-hl--highlight-region beg end nil)
  (when tsx-mode--current-css-region
    (tsx-mode--fontify-current-css-region))
  `(jit-lock-bounds
    ,(min beg (or (car tsx-mode--current-css-region) (point-max)))
    . ,(max end (or (cdr tsx-mode--current-css-region) (point-min)))))


(defun tsx-mode--fontify-current-css-region ()
  "Internal function.

Farm out syntax highlighting of CSS to a separate buffer."
  (message "fontifying current region")
  (let* ((region tsx-mode--current-css-region)
         (beg (max (point-min) (car region)))
         (end (min (point-max) (cdr region)))
         (str (buffer-substring beg (- end 1)))
         (fontified-text-properties-list nil))
    ;; get fontification properties to apply by font-locking our secret buffer
    (with-current-buffer tsx-mode--css-buffer
      ;; --fontify-current-css-region is called in the context of a post-command
      ;; hook which means `inhibit-modification-hooks' is temporarily set to non-
      ;; nil.  but that will prevent desirable side-effects from occurring in our
      ;; CSS buffer so turn it off for a little while
      (message "inserting: %d-%d" beg (- end 1))
      (let ((inhibit-modification-hooks nil))
        (erase-buffer)
        ;; wrap the inserted text in a dummy CSS selector.  this allows us to properly
        ;; calculate indentation as well as get capf to return everything we want it to
        (insert (format "div{%s}" str))
        (tree-sitter--after-change (point-min) (point-max) 0)
        (font-lock-fontify-buffer))
      (setq fontified-text-properties-list
            (object-intervals
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
  (tsx-mode--update-current-css-region))


(defun tsx-mode--do-css-region-change (old-region new-region)
  "Internal function.

Runs the exit-CSS-region hook with OLD-REGION, then the enter-CSS-region hook with NEW-REGION, then returns NEW-REGION."
  (unless (or (= (car new-region) (car old-region))
              (= (cdr new-region) (cdr new-region)))
    ;; don't run hooks if the region is the same but its bounds have changed
    (message "changing css-in-js regions")
    (run-hook-with-args 'tsx-mode-css-exit-region-hook old-region)
    (run-hook-with-args 'tsx-mode-css-enter-region-hook new-region))
  new-region)


(defun tsx-mode--do-css-region-enter (new-region)
  "Internal function.

Runs the enter-CSS-region hook with NEW-REGION, then returns NEW-REGION."
  (message "entering css-in-js region")
  (run-hook-with-args 'tsx-mode-css-enter-region-hook new-region)
  new-region)

  
(defun tsx-mode--do-css-region-exit (old-region)
  "Internal function.

Runs the exit-CSS-region hook with OLD-REGION, then returns OLD-REGION."
  (message "exiting css-in-js region")
  (run-hook-with-args 'tsx-mode-css-exit-region-hook old-region)
  old-region)

  
(defun tsx-mode--update-current-css-region ()
  "Internal function.

Detect changes to the current CSS-in-JS region, and update state and run hooks if necessary."
  (setq
   tsx-mode--current-css-region
   (let ((old-region tsx-mode--current-css-region)
         (new-region (tsx-mode--css-region-for-point)))
     (message "old: %s, new: %s" old-region new-region)
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
  (message "after change %d" (point))
  (message "after change %s" tsx-mode--css-regions)
  (tsx-mode--css-parse-buffer)
  (message "after parse %s" tsx-mode--css-regions)
  (tsx-mode--update-current-css-region)
  (message "after update %s" tsx-mode--current-css-region)
  (when tsx-mode--current-css-region
    (tsx-mode--fontify-current-css-region)))


(defun tsx-mode--indent-css-at-pos (css-buffer-pos)
  "Internal function.

Calculate indentation for line CSS-BUFFER-LINE in the CSS-in-JS buffer."
  (tsi--indent-line-to
   (with-current-buffer tsx-mode--css-buffer
;     (setq-local tsi-debug t)
     (goto-char css-buffer-pos)
     (message "indenting: >%s<" (string (char-after)))
     (message "amount?: %d" (tsi--walk 'tsi-css--get-indent-for))
     (tsi--walk 'tsi-css--get-indent-for))))


(defun tsx-mode--indent-line ()
  "Internal function.

Calculate indentation for the current line."
  (if (< (save-excursion
           (beginning-of-line)
           (point))
         (car tsx-mode--current-css-region))
      ;; point is in region, but the line itself is not
      (tsi-typescript--indent-line)
    (tsx-mode--indent-css-at-pos
     (+ 1 (length "div{") (- (point) (car tsx-mode--current-css-region))))))


(defun tsx-mode--css-enter-region (new-region)
  "Internal function.

A hook function registered at `tsx-mode-css-enter-region-hook'."
  (setq-local indent-line-function #'tsx-mode--indent-line)
  (jit-lock-refontify (car new-region) (cdr new-region)))


(defun tsx-mode--css-exit-region (old-region)
  "Internal function.

A hook function registered at `tsx-mode-css-exit-region-hook'."
  (setq-local indent-line-function #'tsi-typescript--indent-line)
  (jit-lock-refontify (car old-region) (cdr old-region)))


(defun tsx-mode--completion-at-point ()
  "Internal function.

Proxy to either scss-mode's capf or lsp-mode's capf depending on where point is."
  (message "tsx-mode capf with region: %s" tsx-mode--current-css-region)
  (if tsx-mode--current-css-region
      (let ((point-offset (+ 1
                             (length "div{")
                             (- (point) (car tsx-mode--current-css-region)))))
        (with-current-buffer tsx-mode--css-buffer
          (goto-char point-offset)
          (message "point is at %s|%s" (string (char-before)) (string (char-after)))
          (message "capf is %s" (css-completion-at-point))
          (css-completion-at-point)))
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
    (message "setting up css buffer...")
    (setq tsx-mode--css-buffer (get-buffer-create " *tsx-mode css*"))
    (with-current-buffer tsx-mode--css-buffer
      (scss-mode)
      (tsi-css-mode)
      (tree-sitter-hl-mode)
      (lsp)))

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
    (setq tsx-mode-debug t)
    (tsx-mode--setup))


;;;###autoload
(with-eval-after-load 'tree-sitter-langs
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx))
  (add-to-list 'tree-sitter-major-mode-language-alist '(scss-mode . css)))


;;;###autoload
(with-eval-after-load 'lsp-mode
  (add-to-list
   'lsp-language-id-configuration
   '(tsx-mode . "typescript")))


(provide 'tsx-mode)

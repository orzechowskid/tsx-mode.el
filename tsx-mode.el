;;; tsx-mode.el --- a batteries-included major mode for JSX and friends -*- lexical-binding: t

;;; Version: 1.1.0

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsx-mode.el

;;; Package-Requires: ((emacs "28.1") (tsi "1.0.0") (tree-sitter-langs "0.11.3"))

;;; Code:


(unless (fboundp 'object-intervals)
  (error "Unsupported: tsx-mode.el requires Emacs 28.1+"))


(require 'js)
(require 'lsp-mode)
(require 'tree-sitter)
(require 'tree-sitter-hl)
(require 'tree-sitter-langs)
(require 'tsi-css)
(require 'tsi-typescript)


(with-eval-after-load 'tree-sitter-langs
  (tree-sitter-require 'tsx)
  (add-to-list 'tree-sitter-major-mode-language-alist '(tsx-mode . tsx))
  (add-to-list 'tree-sitter-major-mode-language-alist '(scss-mode . css)))
(with-eval-after-load 'lsp-mode
  (add-to-list
   'lsp-language-id-configuration
   '(tsx-mode . "typescript")))


(defvar tsx-mode-css-region-delimiters
  '((
     ;; styled-components, emotion, etc.
     :start "\\(styled\\|css\\)[.()<>[:alnum:]]?+`"
     :start-offset 0
     :end "`;"
     :end-offset -1)
    (
     ;; styled-jsx
     :start "<style jsx[[:blank:][:alnum:]]?+>{`"
     :start-offset 0
     :end "`}"
     :end-offset -1))
  "A list of information defining CSS-in-JS regions.

Each CSS-in-JS mode definition is a plist containing the following properties:

:start - a string defining a regular expression for finding the beginning of a region
:start-offset - a number defining the offset from the end of :start at which the region should begin
:end - a string defining a regular expression for finding the end of a region
:end-offset - a number defining the offset from the end of :end at which the region should end.")


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

List of all CSS-in-JS regions in this buffer.")


(defun tsx-mode--find-css-regions (region-def)
  "Internal function.

Find CSS-in-JS regions defined by REGION-DEF and adds them to `tsx-mode--css-regions."
  (save-excursion
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
           tsx-mode--css-regions))))))


(defun tsx-mode--css-parse-buffer ()
  "Internal function.

Parse the buffer from top to bottom for each entry in the region-definition list."
  (setq tsx-mode--css-regions '())
  (dolist (region-def tsx-mode-css-region-delimiters)
    (tsx-mode--find-css-regions region-def))
  (tsx-mode--debug "CSS regions: %s" tsx-mode--css-regions))


(defun tsx-mode--css-region-for-point ()
  "Internal function.

Get the region at point (if any)."
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
  (tree-sitter-hl--highlight-region beg end nil)
  (when tsx-mode--current-css-region
    (tsx-mode--fontify-current-css-region))
  `(jit-lock-bounds
    ,(min beg (or (car tsx-mode--current-css-region) (point-max)))
    . ,(max end (or (cdr tsx-mode--current-css-region) (point-min)))))


(defun tsx-mode--fontify-current-css-region ()
  "Internal function.

Perform syntax highlighting of CSS in a separate buffer."
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

Run the exit-CSS-region hook with OLD-REGION, then the enter-CSS-region hook with NEW-REGION, then returns NEW-REGION."
  (unless (or (= (car new-region) (car old-region))
              (= (cdr new-region) (cdr new-region)))
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

Detect changes to the current CSS-in-JS region, and update state and run hooks if necessary."
  (setq
   tsx-mode--current-css-region
   (let ((old-region tsx-mode--current-css-region)
         (new-region (tsx-mode--css-region-for-point)))
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
  (tsx-mode--css-parse-buffer)
  (tsx-mode--update-current-css-region)
  (when tsx-mode--current-css-region
    (tsx-mode--fontify-current-css-region)))


(defun tsx-mode--indent-css-at-pos (css-buffer-pos)
  "Internal function.

Calculate indentation for line CSS-BUFFER-LINE in the CSS-in-JS buffer."
  (tsi--indent-line-to
   (with-current-buffer tsx-mode--css-buffer
     ;;     (setq-local tsi-debug t)
     (goto-char css-buffer-pos)
     (tsi--walk 'tsi-css--get-indent-for))))


(defun tsx-mode--indent-line ()
  "Internal function.

Calculate indentation for the current line."
  (if (< (save-excursion
           (beginning-of-line)
           (point))
         (car tsx-mode--current-css-region))
      ;; point is in a CSS region but the line itself is not
      (tsi-typescript--indent-line)
    (tsx-mode--indent-css-at-pos
     (+ 1 (length "div{") (- (point) (car tsx-mode--current-css-region))))))


(defun tsx-mode--css-enter-region (new-region)
  "Internal function.

A hook function registered at `tsx-mode-css-enter-region-hook'."
  (setq-local indent-line-function #'tsx-mode--indent-line)
  ;; don't forget to bounds-check in case the region has shrunk due to a kill command
  (jit-lock-refontify (min (car new-region) (point-max)) (min (cdr new-region) (point-max))))


(defun tsx-mode--css-exit-region (old-region)
  "Internal function.

A hook function registered at `tsx-mode-css-exit-region-hook'."
  (setq-local indent-line-function #'tsi-typescript--indent-line)
  ;; don't forget to bounds-check in case the region has shrunk due to a kill command
  (jit-lock-refontify (min (car old-region) (point-max)) (min (cdr old-region) (point-max))))


(defun tsx-mode--completion-at-point ()
  "Internal function.

Delegate to either css-mode's capf or lsp-mode's capf depending on where point is."
  (if tsx-mode--current-css-region
      (let* ((point-offset (+ 1
                              (length "div{")
                              (- (point) (car tsx-mode--current-css-region))))
             (completion
              (with-current-buffer tsx-mode--css-buffer
                (goto-char point-offset)
                (css-completion-at-point))))
        (if completion
            (let ((offset (+ (car tsx-mode--current-css-region)
                             (- (+ 1 (length "div{"))))))
              ;; translate css-buffer coordinates into main-buffer coordinates
              (setcar (nthcdr 1 completion)
                      (+ (cadr completion) offset))
              (setcar (nthcdr 0 completion)
                      (+ (car completion) offset))
              completion)
          nil))
    (lsp-completion-at-point)))


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
    (setq-local comment-start "// ")
    (setq-local comment-end "")
    ;; re-use the existing TS[X] language data shipped with tree-sitter
    (setq tree-sitter-hl-default-patterns
          (tree-sitter-langs--hl-default-patterns 'tsx))

    (tsi-typescript-mode)
    (tree-sitter-hl-mode)
    (tsx-mode--css-parse-buffer)

    (lsp-ensure-server 'ts-ls)
    ;; TODO: would a CSS langserver be useful here?

    (lsp)

    (unless tsx-mode--css-buffer
      (message "setting up css buffer...")
      (setq tsx-mode--css-buffer (get-buffer-create " *tsx-mode css*"))
      (with-current-buffer tsx-mode--css-buffer
        (scss-mode)
        ;; use scss-mode's native highlighting since tree-sitter's does not look good
        ;;      (tree-sitter-hl-mode)
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
    (add-hook
     'completion-at-point-functions
     #'tsx-mode--completion-at-point
     nil t))


(provide 'tsx-mode)

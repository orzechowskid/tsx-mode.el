;;; graphql.el --- GraphQL support for tsx-mode -*- lexical-binding: t -*-

;;; Author: Dan Orzechowski

;;; URL: https://github.com/orzechowskid/tsx-mode.el

;;; Code:


(require 'graphql-mode)


(defcustom tsx-mode-gql-force-highlighting nil
  "When set to t, GraphQL tagged-template strings will have syntax highlighting
applied to them even if point is no longer inside of them."
  :type 'boolean
  :group 'tsx-mode)

(defvar tsx-mode-gql-region-delimiters
  '((:start "gql`"
     :start-offset 0
     :end "`;"
     :end-offset -1))
  "A list of information defining GraphQL regions.

Each GraphQL mode definition is a plist containing the following properties:

:start - a string defining a regular expression for finding the beginning of a
         region
:start-offset - a number defining the offset from the end of :start at which the
                region should begin
:end - a string defining a regular expression for finding the end of a region
:end-offset - a number defining the offset from the end of :end at which the
               region should end.")

(defvar-local tsx-mode-gql-enter-region-hook
    nil
  "A hook which gets run when point is leaving a GraphQL region.")

(defvar-local tsx-mode-gql-exit-region-hook
    nil
  "A hook which gets run when point is entering a GraphQL region.")

(defvar tsx-mode--gql-buffer
    nil
  "Internal variable.
Super secret buffer for performing gql-related tasks.")

(defvar-local tsx-mode--current-gql-region
    nil
  "Internal variable.
GraphQL region containing point (if any).")

(defvar-local tsx-mode--gql-regions
    '()
  "Internal variable.
Plist of all GraphQL regions in this buffer.")


(defun tsx-mode--gql-get-regions-for-def (region-def)
  "Internal function.
Find and return GraphQL regions in this buffer defined by REGION-DEF."
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
                                 (point-max)))
             regions-for-def)))))
    regions-for-def))

(defun tsx-mode--gql-update-regions ()
  "Internal function.
Update the list of GraphQL regions in the current buffer, and optionally call
for re-fontification if things have changed a lot."
  (let ((next-region-list
         (apply
          'append
          (mapcar
           'tsx-mode--gql-get-regions-for-def
           tsx-mode-gql-region-delimiters))))
    (tsx-mode--debug "gql regions: %s" next-region-list)
    (when (not (eq (length next-region-list)
                   (length tsx-mode--gql-regions)))
      ;; TODO: this check isn't as smart as it could be
      (jit-lock-refontify (point-min) (point-max)))
    (setq tsx-mode--gql-regions next-region-list)))

(defun tsx-mode--gql-region-for-point ()
  "Internal function.
Get the region at point (if any)."
  (seq-find
   (lambda (elt)
     (and
      (>= (point) (plist-get elt :region-begin))
      (< (point) (plist-get elt :region-end))))
   tsx-mode--gql-regions
   nil))

(defun tsx-mode--gql-region-for-line ()
  "Internal function.
Get the region beginning on, ending on, or including the line number at point
(if any)."
  (or
   tsx-mode--current-gql-region
   (seq-find
    (lambda (elt)
      (and (>= (line-number-at-pos)
               (line-number-at-pos (plist-get elt :region-start)))
           (<= (line-number-at-pos)
               (line-number-at-pos) (plist-get elt :region-end))))
    tsx-mode--gql-regions)))

(defun tsx-mode--fontify-current-gql-region ()
  "Internal function.
Perform syntax highlighting of GQL in a separate buffer then copy text
properties back to this buffer."
  (tsx-mode--debug "fontify gql")
  (let* ((region tsx-mode--current-gql-region)
         (beg (max (point-min) (plist-get region :region-begin)))
         (end (min (point-max) (plist-get region :region-end)))
         (str (buffer-substring-no-properties beg (- end 1)))
         (fontified-text-properties-list
          (with-current-buffer tsx-mode--gql-buffer
            (unless (string= str (buffer-string))
              (let ((inhibit-modification-hooks nil))
                (erase-buffer)
                (insert str)
                (font-lock-ensure (point-min) (point-max))))
            (maybe-object-intervals
             (buffer-substring
              (point-min)
              (point-max))))))
    ;; apply those fontification properties to this buffer
    (when fontified-text-properties-list
      (with-silent-modifications
        (dolist (range-with-property fontified-text-properties-list)
          ;; each list entry is '(range-start-pos range-end-pos (plist))
          (set-text-properties
           (+ beg (elt range-with-property 0))
           (+ beg (elt range-with-property 1))
           (elt range-with-property 2)))))))

(defun tsx-mode--do-gql-region-change (old-region new-region)
  "Internal function.
Run the exit-gql-region hook with OLD-REGION, then the enter-gql-region hook
with NEW-REGION, then returns NEW-REGION."
  (unless (or (=
               (plist-get new-region :region-begin)
               (plist-get old-region :region-begin))
              (=
               (plist-get new-region :region-end)
               (plist-get old-region :region-end)))
    ;; don't run hooks if the region is the same but its bounds have changed
    (tsx-mode--debug "changing gql regions")
    (run-hook-with-args 'tsx-mode-gql-exit-region-hook old-region)
    (run-hook-with-args 'tsx-mode-gql-enter-region-hook new-region))
  new-region)

(defun tsx-mode--do-gql-region-enter (new-region)
  "Internal function.
Run the enter-gql-region hook with NEW-REGION, then returns NEW-REGION."
  (tsx-mode--debug "entering gql region")
  (run-hook-with-args 'tsx-mode-gql-enter-region-hook new-region)
  new-region)

(defun tsx-mode--do-gql-region-exit (old-region)
  "Internal function.
Run the exit-gql-region hook with OLD-REGION, then returns OLD-REGION."
  (tsx-mode--debug "exiting gql region")
  (run-hook-with-args 'tsx-mode-gql-exit-region-hook old-region)
  old-region)

(defun tsx-mode--update-current-gql-region ()
  "Internal function.
Detect changes to the current GraphQL region, and update state and run hooks
if necessary."
  (setq
   tsx-mode--current-gql-region
   (let ((old-region tsx-mode--current-gql-region)
         (new-region (tsx-mode--gql-region-for-point)))
     (tsx-mode--debug "gql old region: %s new region: %s" old-region new-region)
     (cond
       ((and old-region new-region)
        (tsx-mode--do-gql-region-change old-region new-region)
        new-region)
       (new-region
        (tsx-mode--do-gql-region-enter new-region)
        new-region)
       (old-region
        (tsx-mode--do-gql-region-exit old-region)
        nil)
       (t nil)))))

(defun tsx-mode--indent-gql-at-pos (gql-buffer-pos)
  "Internal function.
Calculate indentation for line GQL-BUFFER-POS in the GraphQL buffer."
  (let ((line-offset
         (- (line-number-at-pos)
            (line-number-at-pos
             (plist-get tsx-mode--current-gql-region :region-begin)))))
    (with-current-buffer tsx-mode--gql-buffer
      (goto-line (1+ line-offset))
      (graphql-indent-line)
      (end-of-line)
      (back-to-indentation)
      (+ (current-column)
         (if (= line-offset 1) ; graphql-mode doesn't properly indent the first line
             graphql-indent-level 
           0)))))

(defun tsx-mode--gql-enter-region (new-region)
  "Internal function.
A hook function registered at `tsx-mode-gql-enter-region-hook'."
  ;; don't forget to bounds-check in case the region has shrunk due to a kill
  (jit-lock-refontify
   (min (plist-get new-region :region-begin) (point-max))
   (min (plist-get new-region :region-end) (point-max))))

(defun tsx-mode--gql-exit-region (old-region)
  "Internal function.
A hook function registered at `tsx-mode-gql-exit-region-hook'."
  ;; don't forget to bounds-check in case the region has shrunk due to a kill
  (jit-lock-refontify
   (min (plist-get old-region :region-begin) (point-max))
   (min (plist-get old-region :region-end) (point-max))))

(defun tsx-mode--gql-completion-at-point ()
  "Internal function.
Perform completion-at-point inside the hidden GQL buffer and apply to this one."
  (let* ((point-offset
          (plist-get tsx-mode--current-gql-region :region-begin))
         (completion
          (with-current-buffer tsx-mode--gql-buffer
            (goto-char point-offset)
            (graphql-completion-at-point))))
    ;; translate gql-buffer coordinates into main-buffer coordinates
    (setcar (nthcdr 1 completion)
            (cadr completion))
    (setcar (nthcdr 0 completion)
            (car completion))
    completion))

(define-minor-mode tsx-mode-gql
  "A tsx-mode minor mode for GraphQL."
  :delight nil
  :group 'tsx-mode
  (unless tsx-mode--gql-buffer
    (tsx-mode--debug "setting up gql buffer...")
    (setq tsx-mode--gql-buffer
          (get-buffer-create " *tsx-mode gql*"))
    (with-current-buffer tsx-mode--gql-buffer
      (graphql-mode)))
  (add-hook
   'tsx-mode-gql-exit-region-hook
   'tsx-mode--gql-exit-region
   nil t)
  (add-hook
   'tsx-mode-gql-enter-region-hook
   'tsx-mode--gql-enter-region
   nil t)
  (add-hook
   'post-command-hook
   'tsx-mode--update-current-gql-region
   nil t)
  (add-hook
   'after-change-functions
   (lambda (beg end old-text-length)
     (tsx-mode--gql-update-regions)
     (tsx-mode--update-current-gql-region))
   nil t)
  (tsx-mode--gql-update-regions))

(require 'company)
(require 'cl-lib)

(defgroup company-dwim nil
  "Another tab-and-go implementation for company, like
`ac-dwim'."
  :group 'company-dwim)

(defcustom company-dwim-trim-newline t
  "When non-nil, preview only first line when completing
  multi-line candidates."
  :type 'boolean
  :group 'company-dwim)

;; ---- state

(defvar company-dwim-half-committed nil)

(defun company-dwim-should-commit-p ()
  (and company-dwim-half-committed
       (or (eq this-command 'company-abort)
           (not (and (symbolp this-command)
                     (string-match-p "\\`company-" (symbol-name this-command)))))))

(defun company-dwim-restore-state (&optional _)
  (setq company-dwim-half-committed nil))
(add-hook 'company-after-completion-hook 'company-dwim-restore-state)

;; ---- the frontend

(defun company-dwim-expand-prefix-p ()
  (and company-common
       (or (eq (company-call-backend 'ignore-case) 'keep-prefix)
           (string-prefix-p company-prefix company-common))
       (not (string= company-prefix company-common))
       (not (company-require-match-p))))

(defvar company-dwim-overlay nil)

(defun company-dwim-overlay-hide ()
  (when company-dwim-overlay
    (delete-overlay company-dwim-overlay)
    (setq company-dwim-overlay nil)))

(defun company-dwim-overlay-show-at-point (pos prefix completion)
  (let ((len (length prefix)))
    (cond ((zerop len)
           (setq company-dwim-overlay (make-overlay (- (point) 1) (point)))
           (overlay-put company-dwim-overlay 'after-string
                        (propertize completion 'face 'company-preview)))
          (t
           (setq company-dwim-overlay (make-overlay (- (point) len) (point)))
           (overlay-put company-dwim-overlay 'display completion)
           (overlay-put company-dwim-overlay 'face 'company-preview)))))

(defun company-dwim-maybe-trim-newline (candidate)
  (if (not company-dwim-trim-newline) candidate
    (car (split-string candidate "\n" t))))

(defun company-dwim-frontend (command)
  (cl-case command
    (pre-command
     (company-preview-hide)
     (company-dwim-overlay-hide)
     (when (company-dwim-should-commit-p)
       (company-complete-selection)))
    (post-command
     (cond ((company-dwim-expand-prefix-p)
            (company-preview-show-at-point (point) company-common))
           (company-dwim-half-committed
            (company-dwim-overlay-show-at-point
             (point) company-prefix
             (company-dwim-maybe-trim-newline (nth company-selection company-candidates))))
           (t
            (let ((company-common (and company-common
                                       (company-dwim-maybe-trim-newline company-common))))
              (company-preview-show-at-point
               (point)
               (company-dwim-maybe-trim-newline (nth company-selection company-candidates)))))))
    (hide
     (company-preview-hide)
     (company-dwim-overlay-hide))))

;; ---- the command

(defun company-dwim (&optional arg)
  (interactive "p")
  (cond ((company-dwim-expand-prefix-p)
         (company-complete-common)
         (setq company-dwim-half-committed nil))
        ((not company-dwim-half-committed)
         (setq company-dwim-half-committed t))
        (t
         (company-select-next arg))))

(defun company-dwim-select-next (&optional arg)
  (interactive "p")
  (unless company-dwim-half-committed
    (setq company-dwim-half-committed t))
  (company-select-next arg))

(defun company-dwim-select-previous (&optional arg)
  (interactive "p")
  (company-dwim-select-next (if arg (- arg) -1)))

(provide 'company-dwim)

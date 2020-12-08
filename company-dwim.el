(require 'company)
(require 'cl-lib)

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

(defun company-dwim-available-p ()
  (and company-common
       (or (eq (company-call-backend 'ignore-case) 'keep-prefix)
           (string-prefix-p company-prefix company-common))))

(defun company-dwim-expand-prefix-p ()
  (and company-common
       (or (eq (company-call-backend 'ignore-case) 'keep-prefix)
           (string-prefix-p company-prefix company-common))
       (not (string= company-prefix company-common))))

(defun company-dwim-preview-show-at-point (pos completion &optional no-keep-cursor)
  "Like `company-preview-show-at-point' but does not preserve
cursor position when NO-KEEP-CURSOR is true."
  (company-preview-show-at-point pos completion)
  (when no-keep-cursor
    (let* ((after-string (overlay-get company-preview-overlay 'after-string))
           (display (overlay-get company-preview-overlay 'display))
           (str (or after-string display)))
      (unless (string= str "")
        (remove-text-properties 0 1 '(cursor) str))
      (overlay-put company-preview-overlay (if after-string 'after-string 'display) str))))

(defun company-dwim-frontend (command)
  (cl-case command
    (pre-command
     (company-preview-hide)
     (when (company-dwim-should-commit-p)
       (company-complete-selection)))
    (post-command
     (cond ((company-dwim-expand-prefix-p)
            (company-preview-show-at-point (point) company-common))
           ((company-dwim-available-p)
            (company-dwim-preview-show-at-point
             (point) (nth company-selection company-candidates)
             company-dwim-half-committed))))
    (hide (company-preview-hide))))

;; ---- the command

(defun company-dwim (&optional arg)
  (interactive "p")
  (cond ((company-dwim-expand-prefix-p)
         (company-complete-common)
         (setq company-dwim-half-committed nil))
        ((and (not company-dwim-half-committed)
              (company-dwim-available-p))
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

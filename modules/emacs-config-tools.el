;;;; Copilot
(require 'copilot)

(defun custom/copilot-tab ()
  "Tab command that will complet with copilot if a completion is
available. Otherwise will try normal
tab-indent."
  (interactive)
  (if (copilot--overlay-visible)
      (copilot-accept-completion))
  (copilot-complete))

(define-key copilot-mode-map (kbd "<backtab>") 'custom/copilot-tab)
(define-key copilot-mode-map (kbd "M-k") #'copilot-next-completion)
(define-key copilot-mode-map (kbd "M-j") #'copilot-previous-completion)

(with-eval-after-load
  (global-copilot-mode))


;;;; Common flycheck
(defun custom/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (if-let ((window (flycheck-get-error-list-window)))
      (save-selected-window (quit-window nil window))
    (flycheck-list-errors)))

(defun custom/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (if (flycheck-get-error-list-window)
      (switch-to-buffer flycheck-error-list-buffer)
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))

(provide 'emacs-config-tools)

;; Associated zig files with zig-mode
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))

(add-hook 'zig-mode-hook #'eglot-ensure)
(add-hook 'zig-mode-hook 'flycheck-mode)
(add-hook 'zig-mode-hook #'hs-minor-mode)

(with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '((zig-mode) "zls")))

(provide 'ide-mode-zig)
